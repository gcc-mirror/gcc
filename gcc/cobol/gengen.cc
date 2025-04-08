/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*  The compilation process consists of

    1) lexing
    2) parsing
    3) generation of the GENERIC abstract syntax tree
    4) reduction
    5) generation of machine code

    For your sins, you have wandered into the code that accepts information from
    the parser about what the COBOL source code wants done.

    Specifically, the routines in this module, which run at compile time, generate
    the GENERIC tags that describe the equivalent of the COBOL.  They are rathernnn
    low level routines, ultimately used for pretty much everything.  Specifically,
    they run at compile-time, and they generate the GENERIC tags that control what
    ultimately happens at run-time.

    It *is* confusing.

    I'll try to collect things in a logical way, and name them in a logical way,
    and I'll try to comment them well enough so that you have some hope of
    understanding what the heck is going on.

    There is some information in the GCC internals document, but it was written by
    people who live and breathe this stuff, and they don't remember what it was like
    to know nothing.

    I suspect that those who have tried and failed to create GCC front ends have foundered because
    they just couldn't figure out what it was they needed to do.  I certainly floundered
    for several days before I hit on the means to figure it out.  I created the
    rjd_print_tree() routine, which spits out a text listing of all the nodes
    connected to the specified starting node.  (Keep in mind that the GENERIC graph
    is cyclic, and consequently there is no real ordering, except that the starting
    node you specify is NodeNumber0.  rjd_print_tree follows all links, but it prints
    out each unique node exactly once.)

    I then built into GCC a call to rjd_print_tree right at the point where the GENERIC tree
    is complete and about to be reduced.

    And that gave me the ability to create simple C programs and see the resulting GENERIC
    tree.  It took a while to sort out what I was seeing, but ultimately things started
    to make sense.  The inherent difficulty may start to become clear when you realize that
    the program

        void foo()
            {
            }

    is implemented by a GENERIC tree with fifty-six nodes.

    I can't try to write a whole manual here.  But hopefully there will be enough examples
    throughout the code for you to learn how to do things on a highish level, and you can
    look at the low -level routines to see how it is accomplished.

    That said, I will try to comment things well enough to be meaningful at least to me
    when I run across them at some time in the future.  Because I fear that whatever
    I do here, the world will little note, and *I* will not long remember, what it was!
    */

#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "cgraph.h"
#include "toplev.h"
#include "function.h"
#include "fold-const.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "gengen.h"
#include "dumpfile.h"

// We are limiting the programmer to functions with 512 or fewer arguments.
// Don't like it?  Cry me a river.
static const int ARG_LIMIT = 512;

static int sv_current_line_number;

// These are globally useful constants
tree char_nodes[256];

tree pvoid_type_node;
tree integer_minusone_node;
tree integer_two_node;
tree integer_eight_node;
tree size_t_zero_node;
tree int128_zero_node;
tree int128_five_node;
tree int128_ten_node;
tree char_ptr_type_node;
tree uchar_ptr_type_node;
tree wchar_ptr_type_node;
tree long_double_ten_node;
tree sizeof_size_t;
tree sizeof_pointer;

tree bool_true_node;
tree bool_false_node;

// This is the global translation unit structure; it contains everything needed
// to compile one file that you might otherwise be tempted to instantiate as
// global variables:

struct cbl_translation_unit_t gg_trans_unit;

void
gg_build_translation_unit(const char *filename)
  {
  // The translation_unit_decl gets declared once for each processing source
  // input file.  It serves as an anchor for each function.  And the
  // block referred to by its "initial" member is the anchor for any
  // variables whose scope is file.

  gg_trans_unit.trans_unit_decl
    = build_translation_unit_decl(get_identifier(filename));

  gg_trans_unit.filename = filename;

  tree tree_block = make_node(BLOCK);
  BLOCK_SUPERCONTEXT(tree_block)
    = gg_trans_unit.trans_unit_decl;
  TREE_USED(tree_block) = 1;
  DECL_INITIAL(gg_trans_unit.trans_unit_decl) = tree_block;
  }

// Explanation of context.  There is a plate of spaghetti that represents
// a chain of contexts.

// The deconstructed dinner:  The function_decl "initial" points to a block
// The block points to the first of a chained set of var_decl, one for each
// variable in the block.  The function "saved_tree" entry points to a
// bind_expr.  The bind_expr vars member points to the same chain of var_decl.
// The bind_expr block member points to the block.  And the bind_expr body
// member points to the statement_list for the context.

// Those four tags constitute the context.  To push the context, a new block
// is chained to the first blocks SUBCHAIN member.  A new bind_expr is created
// and put on the statement_list of the enclosing block.  And a new list of
// var_decls is set up for the new block and the new bind_expr.

// And that's how subcontexts are made.

static void
gg_chain_onto_block_vars(tree block, tree var)
  {
  // In order to use a variable in a context, the var_decl has to go
  // onto the chain that starts with the "vars" entry of a block

  // Upon discovering that chainon has O(N-squared) complexity because it walks
  // the entire chain looking for the final member, Dubner put in this map.
  static std::unordered_map<tree, tree>blocks;
  if( !BLOCK_VARS(block) )
    {
    // This is the first variable:
    BLOCK_VARS(block) = var;
    blocks[block] = var;
    }
  else
    {
    //chainon(BLOCK_VARS(block), var);
    // What follows is the quicker equivalent of calling chainon()
    TREE_CHAIN(blocks[block]) = var;
    blocks[block] = var;
    }
  }

void
gg_append_var_decl(tree var_decl)
  {
  // The var_decl has to be chained onto the appropriate block.

  if( SCOPE_FILE_SCOPE_P(DECL_CONTEXT(var_decl)) )
    {
    tree context = gg_trans_unit.trans_unit_decl;
    tree block = DECL_INITIAL(context);

    gg_chain_onto_block_vars(block, var_decl);

    rest_of_decl_compilation (var_decl, true, false);

    // With global variables, it is probably necessary to do something with
    // wrapup_global_declarations.  At this writing, I have not yet
    // investigated that.  The advice from gcc@gcc.gnu.org came from
    // David Malcolm:
    /*
    You might find libgccjit's gcc/jit/jit-playback.cc helpful for this, as
    it tends to contain minimal code to build trees (generally
    simplified/reverse-engineered from the C frontend).

    playback::context::global_new_decl makes the VAR_DECL node, and such
    trees are added to the jit playback::context's m_globals.
    In playback::context::replay, we have:

      / * Finalize globals. See how FORTRAN 95 does it in gfc_be_parse_file()
         for a simple reference. * /
      FOR_EACH_VEC_ELT (m_globals, i, global)
        rest_of_decl_compilation (global, true, true);

      wrapup_global_declarations (m_globals.address(), m_globals.length());
    */

    // Stash this var_decl in a map so it can be found elsewhere:
    //fprintf(stderr, "Stashing %s\n", IDENTIFIER_POINTER(DECL_NAME(var_decl)));
    gg_trans_unit.trans_unit_var_decls
    [IDENTIFIER_POINTER(DECL_NAME(var_decl))] = var_decl;
    }
  else
    {
    // For function-level variables, we use a stack of blocks to keep track
    // of which block is active for the current context:

    // fprintf(stderr, "%s():  %30s Function Scope\n", __func__, id_name);
    tree bind_expr = current_function->bind_expr_stack.back();
    tree block = BIND_EXPR_BLOCK(bind_expr);

    gg_chain_onto_block_vars(block, var_decl);

    // If saved_tree.bind_expr.vars is null, then var_decl is the very
    // first variable in the block, and it must be set in bind_expr as well
    if( !BIND_EXPR_VARS(bind_expr)  )
      {
      BIND_EXPR_VARS(bind_expr) = var_decl;
      }
    }
  }

location_t
location_from_lineno()
  {
  location_t loc;
  loc = linemap_line_start(line_table, sv_current_line_number, 0);
  return loc;
  }

void
gg_append_statement(tree stmt)
  {
  // Likewise, we have a stack of statement_lists, with the current one
  // at the back.  (The statement_list stack can get deeper than the block
  // stack, because you can create a separate statement list for the insides
  // of, say, a WHILE statement without creating a whole context for it)

  // This statement list thing looks innocent enough, but it is the general
  // way of actually having a GENERIC tree generate executing code.  What goes
  // onto a statement list is an expression.  A = B is implemented with a
  // modify_expr

  // Actually instantiating a variable requires a var_expr

  // A subroutine call is effected by putting a call_expr onto the statement
  // list.

  // It's not the only way; you can have a modify_expr that takes a var_decl
  // as a destination, and uses a call_expr as a source.  This requires that
  // the type of the var_decl be the same as the type of the function being
  // called.

  // And so on.  Just keep in mind that you have types, and declarations, and
  // expressions, among other things.

  // When trying to figure out location_t, take a look at
  // ./libcpp/include/line-map.h
  // ./libcpp/location-example.txt

  gcc_assert(  gg_trans_unit.function_stack.size() );

  TREE_SIDE_EFFECTS(stmt) = 1;    // If an expression has no side effects,
  //                              // it won't generate code.
  TREE_SIDE_EFFECTS(current_function->statement_list_stack.back()) = 1;
  append_to_statement_list( stmt, &(current_function->statement_list_stack.back()) );
  }

tree
gg_float(tree floating_type, tree integer_var)
  {
  // I don't know why, but this fails if 'var' is an INT128
  return build1(FLOAT_EXPR, floating_type, integer_var);
  }

tree
gg_trunc(tree integer_type, tree floating_var)
  {
  /* Conversion of real to fixed point by truncation.  */
  return build1(FIX_TRUNC_EXPR, integer_type, floating_var);
  }

tree
gg_cast(tree type, tree var)
  {
  return fold_convert(type, var);
  }

static bool saw_pointer;

static
tree
adjust_for_type(tree type)
  {
  tree retval;

  switch( TREE_CODE(type) )
    {
    case POINTER_TYPE:
      saw_pointer = true;
      retval = adjust_for_type(TREE_TYPE(type));
      break;

    case COMPONENT_REF:
    case ADDR_EXPR:
    case ARRAY_TYPE:
    case VAR_DECL:
    case FUNCTION_TYPE:
      retval = adjust_for_type(TREE_TYPE(type));
      break;
    case RECORD_TYPE:
    default:
      retval = type;
      break;
    }

  return retval;
  }

static
char *
show_type(tree type)
  {
  if( !type )
    {
    cbl_internal_error("The given type is not NULL, and that's just not fair");
    }

  if( DECL_P(type) )
    {
    type = TREE_TYPE(type);
    }
  if( !TYPE_P(type) )
    {
    cbl_internal_error("The given type is not a DECL or a TYPE");
    }

  static char ach[1024];
  switch( TREE_CODE(type) )
    {
    case POINTER_TYPE:
      sprintf(ach, "POINTER");
      break;

    case VOID_TYPE:
      sprintf(ach, "VOID");
      break;

    case BOOLEAN_TYPE:
      sprintf(ach, "BOOL");
      break;

    case RECORD_TYPE:
      sprintf(ach, "RECORD");
      break;

    case REAL_TYPE:
      sprintf(ach,
              "%3ld-bit REAL",
              TREE_INT_CST_LOW(TYPE_SIZE(type)));
      break;

    case INTEGER_TYPE:
      sprintf(ach,
              "%3ld-bit %s INT",
              TREE_INT_CST_LOW(TYPE_SIZE(type)),
              (TYPE_UNSIGNED(type) ? "unsigned" : "  signed"));
      break;

    case FUNCTION_TYPE:
      sprintf(ach, "FUNCTION");
//      sprintf(ach,
//              "%3ld-bit %s INT",
//              TREE_INT_CST_LOW(TYPE_SIZE(type)),
//              (TYPE_UNSIGNED(type) ? "unsigned" : "  signed"));
      break;

    default:
      cbl_internal_error("Unknown type %d", TREE_CODE(type));
    }

  return ach;
  }

void
gg_assign(tree dest, const tree source)
  {
  // This does the equivalent of a C/C++ "dest = source".  When X1 is set, it
  // does some checking for conditions that can result in inefficient code, so
  // that is useful during development when even an astute programmer might
  // need an assist with keeping variable types straight.

  // This routine also provides for the possibility that the assignment is
  // for a source that is a function invocation, as in
  //    "dest = function_call()"

  saw_pointer = false;
  tree dest_type = adjust_for_type(TREE_TYPE(dest));
  saw_pointer = false;
  tree source_type = adjust_for_type(TREE_TYPE(source));
  bool p2 = saw_pointer;

  bool okay = dest_type == source_type;

  if( !okay )
    {
    if(    TREE_CODE(dest_type)   == INTEGER_TYPE
           && TREE_CODE(source_type) == INTEGER_TYPE
           && TREE_INT_CST_LOW(TYPE_SIZE(dest_type)) == TREE_INT_CST_LOW(TYPE_SIZE(source_type))
           && TYPE_UNSIGNED(dest_type) == TYPE_UNSIGNED(source_type) )
      {
      okay = true;
      }
    }

  if( okay )
    {
    tree stmt = build2_loc( location_from_lineno(),
                            MODIFY_EXPR,
                            TREE_TYPE(dest),
                            dest,
                            source);
    gg_append_statement(stmt);
    }
  else
    {
    // We are doing an assignment where the left- and right-hand types are not
    // the same.  This is a compilation-time error, since we want the caller to
    // have sorted the types out explicitly.  If we don't throw an error here,
    // the gimple reduction will do so.  Better to do it here, when we know
    // where we are.
    dbgmsg("Inefficient assignment");
    if(DECL_P(dest) && DECL_NAME(dest))
      {
      dbgmsg("   Destination is %s", IDENTIFIER_POINTER(DECL_NAME(dest)));
      }
    dbgmsg("   dest type   is %s%s", show_type(dest_type), p2 ? "_P" : "");
    if(DECL_P(source) && DECL_NAME(source))
      {
      dbgmsg("   Source      is %s", IDENTIFIER_POINTER(DECL_NAME(source)));
      }
    dbgmsg("   source type is %s%s", show_type(source_type), p2 ? "_P" : "");
    gcc_unreachable();
    }
  }

tree
gg_find_field_in_struct(const tree base, const char *field_name)
  {
  // Finds and returns the field_decl for the named member.  'base' can be
  // a structure or a pointer to a structure.
  tree type = TREE_TYPE(base);
  tree rectype;
  if( POINTER_TYPE_P (type) )
    {
    tree pointer_type = TREE_TYPE(base);
    rectype = TREE_TYPE(pointer_type);
    }
  else
    {
    // Assuming a struct (or union), pick up the record_type
    rectype = TREE_TYPE(base);
    }

  tree id_of_field = get_identifier(field_name);

  tree field_decl = NULL_TREE;

  tree next_value = TYPE_FIELDS(rectype);

  // Look through the chain of fields for a match to ours.  This is, in the
  // limit, an O(N^2) computational burden.  But structures usually small, so we
  // probably don't have to figure out how to make it faster.
  while( next_value )
    {
    if( DECL_NAME(next_value) == id_of_field )
      {
      field_decl = next_value;
      break;
      }
    next_value = TREE_CHAIN(next_value);
    }

  if( !field_decl )
    {
    yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
    yywarn("###### Somebody asked for the field %s.%s, which doesn't exist",
          IDENTIFIER_POINTER(DECL_NAME(base)),
          field_name);
    gcc_unreachable();
    }

  return field_decl;
  }

static tree
gg_start_building_a_union(const char *type_name, tree type_context)
  {
  // type_context is current_function->function_decl for union local
  // to a function.

  // It is translation_unit_decl for unions common to all functions

  // We want to return the type_decl for an empty union

  // First, create the record_type whose values will eventually
  // be the chain of of the struct's fields:

  tree uniontype = make_node(UNION_TYPE);
  TYPE_CONTEXT(uniontype) = type_context;
  TYPE_SIZE_UNIT(uniontype) = integer_zero_node;
  TYPE_SIZE(uniontype) = integer_zero_node;
  TYPE_NAME(uniontype) = get_identifier(type_name);

  TYPE_MODE_RAW(uniontype) = TYPE_MODE (intTI_type_node);

  // We need a type_decl for the record_type:
  tree typedecl = make_node(TYPE_DECL);

  // The type of the type_decl is the record_type:
  TREE_TYPE(typedecl) = uniontype;

  SET_TYPE_ALIGN(uniontype, 16);

  // The chain element of the record_type points back to the type_decl:
  TREE_CHAIN(uniontype) = typedecl;

  return typedecl;
  }

static tree
gg_start_building_a_struct(const char *type_name, tree type_context)
  {
  // type_context is current_function->function_decl for structures local
  // to a function.

  // It is translation_unit_decl for structures common to all functions

  // We want to return the type_decl for an empty struct

  // First, create the record_type whose values will eventually
  // be the chain of of the struct's fields:

  tree recordtype = make_node(RECORD_TYPE);
  TYPE_CONTEXT(recordtype) = type_context;
  TYPE_SIZE_UNIT(recordtype) = integer_zero_node;
  TYPE_SIZE(recordtype) = integer_zero_node;
  TYPE_NAME(recordtype) = get_identifier(type_name);

  TYPE_MODE_RAW(recordtype) = BLKmode;

  // We need a type_decl for the record_type:
  tree typedecl = make_node(TYPE_DECL);

  // The type of the type_decl is the record_type:
  TREE_TYPE(typedecl) = recordtype;

  SET_TYPE_ALIGN(recordtype, 8);

  // The chain element of the record_type points back to the type_decl:
  TREE_CHAIN(recordtype) = typedecl;

  return typedecl;
  }

static void
gg_add_field_to_structure(const tree type_of_field, const char *name_of_field, tree struct_type_decl)
  {
  // We're given the struct_type_decl.
  // Append the new field to that type_decl's record_type's chain:
  tree struct_record_type = TREE_TYPE(struct_type_decl);

  bool is_union = TREE_CODE((struct_record_type)) == UNION_TYPE;

  tree id_of_field = get_identifier (name_of_field);

  // Create the new field:
  tree new_field_decl = build_decl(   location_from_lineno(),
                                      FIELD_DECL,
                                      id_of_field,
                                      type_of_field);

  // Establish the machine mode for the field_decl:
  SET_DECL_MODE(new_field_decl, TYPE_MODE(type_of_field));

  // Establish the context of the new field as being the record_type
  DECL_CONTEXT (new_field_decl) = struct_record_type;

  // Establish the size of the new field as being the same as its prototype:
  DECL_SIZE(new_field_decl) = TYPE_SIZE(type_of_field);            // This is in bits
  DECL_SIZE_UNIT(new_field_decl) = TYPE_SIZE_UNIT(type_of_field);  // This is in bytes

  // We need to establish the offset and bit offset of the new node.
  // Empirically, this seems to be done on 16-bit boundaries, with DECL_FIELD_OFFSET
  // in units of N*16 bytes, and FIELD_BIT_OFFSET being offsets in bits from the DECL_FIELD_OFFSET

  // We calculate our desired offset in bits:

  // Pick up the current size, in bytes, of the record_type:
  long offset_in_bytes = TREE_INT_CST_LOW(TYPE_SIZE_UNIT(struct_record_type));

  static const int MAGIC_NUMBER_SIXTEEN = 16 ;
  static const int BITS_IN_A_BYTE = 8 ;

  // We know the offset_in_bytes, which is the size, of the structure with
  // its current members.

  //long type_size  =  TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type_of_field));
  long type_align_in_bits  =  TYPE_ALIGN(type_of_field);
  long type_align_in_bytes = type_align_in_bits/BITS_IN_A_BYTE;

  // As per the Amd64 ABI, we need to set the structure's type alignment to be
  // that of most strictly aligned component:
  // This is the current restriction:
  long struct_align_in_bits  =  TYPE_ALIGN(TREE_TYPE(struct_type_decl));
  if( type_align_in_bits > struct_align_in_bits )
    {
    // The new one is the new champion
    SET_TYPE_ALIGN(TREE_TYPE(struct_type_decl), type_align_in_bits );
    }

  // We know struct_type_decl is a record_type, so we can sneak through this comparison
  if( type_of_field == TREE_TYPE(struct_type_decl) )
    {
    printf("   It is a record_type\n");
    }

  // Bump up the offset until we are aligned:
  while( offset_in_bytes % type_align_in_bytes)
    {
    offset_in_bytes += 1;
    }

  if( is_union )
    {
    // Turn that into the bytes/bits offsets of the new field:
    DECL_FIELD_OFFSET(new_field_decl) = build_int_cst_type (SIZE_T, 0);
    DECL_FIELD_BIT_OFFSET(new_field_decl) = build_int_cst_type (bitsizetype, 0);

    // The size of a union is the size of its largest member:
    offset_in_bytes = std::max(offset_in_bytes, (long)TREE_INT_CST_LOW(DECL_SIZE_UNIT(new_field_decl)));
    }
  else
    {
    // Turn that into the bytes/bits offsets of the new field:
    long field_offset = (offset_in_bytes/MAGIC_NUMBER_SIXTEEN)*MAGIC_NUMBER_SIXTEEN;
    long field_bit_offset = (offset_in_bytes - field_offset) * BITS_IN_A_BYTE;
    DECL_FIELD_OFFSET(new_field_decl) = build_int_cst_type (SIZE_T, field_offset);;
    DECL_FIELD_BIT_OFFSET(new_field_decl) = build_int_cst_type (bitsizetype, field_bit_offset);

    // This was done empirically to make our generated code match that of a C program
    SET_DECL_OFFSET_ALIGN(new_field_decl, 128);

    // And now we need to update the size of the record type:
    offset_in_bytes += TREE_INT_CST_LOW(DECL_SIZE_UNIT(new_field_decl));
    }

  TYPE_SIZE_UNIT(struct_record_type) = build_int_cst_type (SIZE_T, offset_in_bytes);           // In bytes
  TYPE_SIZE(struct_record_type) = build_int_cst_type (bitsizetype, offset_in_bytes*BITS_IN_A_BYTE); // In bits

  if( !TYPE_FIELDS(struct_record_type) )
    {
    // This is the first variable of the chain:
    TYPE_FIELDS(struct_record_type) = new_field_decl;
    }
  else
    {
    // We need to tack the new one onto an already existing chain:
    chainon(TYPE_FIELDS(struct_record_type), new_field_decl);
    }
  }

void
gg_get_struct_type_decl(tree struct_type_decl, int count, va_list params)
  {
  while( count-- )
    {
    tree field_type = va_arg(params, tree);
    const char *name = va_arg(params, const char *);
    gg_add_field_to_structure(field_type, name, struct_type_decl);
    }
  // Note:  On 2022-02-18 I removed the call to gg_append_var_decl, which
  // chains the type_decl on the function block.  I don't remember why I
  // thought it was necessary.  It makes no difference for COBOL compilations.
  //
  // But I must have copied it from a C compilation example.
  //
  // I removed it so that I could create type_decls outside of a function.
  // I know not what the long-term implications might be.
  //
  // You have been served notice.
  //
  // struct_type_decl is the type_decl for our structure.  We need to
  // append it to the list of variables in order to use it:
  // The following function call is misnamed.  It can take struct type_decls
  //gg_append_var_decl(struct_type_decl);
  }

void
gg_get_union_type_decl(tree union_type_decl, int count, va_list params)
  {
  while( count-- )
    {
    tree field_type = va_arg(params, tree);
    const char *name = va_arg(params, const char *);
    gg_add_field_to_structure(field_type, name, union_type_decl);
    }
  }

tree
gg_get_local_struct_type_decl(const char *type_name, int count, ...)
  {
  tree struct_type_decl = gg_start_building_a_struct(type_name, current_function->function_decl);

  va_list params;
  va_start(params, count);

  gg_get_struct_type_decl(struct_type_decl, count, params);

  va_end(params);

  // To use the struct_type_decl, you'll need to execute
  // the following to turn it into a var_decl:
  //    tree var_decl = gg_define_variable( TREE_TYPE(struct_type_decl),
  //                                        var_name,
  //                                        vs_static);
  return struct_type_decl;
  }

tree
gg_get_filelevel_struct_type_decl(const char *type_name, int count, ...)
  {
  tree struct_type_decl = gg_start_building_a_struct(type_name, gg_trans_unit.trans_unit_decl);

  va_list params;
  va_start(params, count);

  gg_get_struct_type_decl(struct_type_decl, count, params);

  va_end(params);

  // To use the struct_type_decl, you'll need to execute
  // the following to turn it into a var_decl:
  //    tree var_decl = gg_define_variable( TREE_TYPE(struct_type_decl),
  //                                        var_name,
  //                                        vs_static);
  return struct_type_decl;
  }

tree
gg_get_filelevel_union_type_decl(const char *type_name, int count, ...)
  {
  tree struct_type_decl = gg_start_building_a_union(type_name, gg_trans_unit.trans_unit_decl);

  va_list params;
  va_start(params, count);

  gg_get_union_type_decl(struct_type_decl, count, params);

  va_end(params);

  // To use the struct_type_decl, you'll need to execute
  // the following to turn it into a var_decl:
  //    tree var_decl = gg_define_variable( TREE_TYPE(struct_type_decl),
  //                                        var_name,
  //                                        vs_static);
  return struct_type_decl;
  }

tree
gg_define_local_struct(const char *type_name, const char * var_name, int count, ...)
  {
  // Builds a structure, declares it as a static variable in the current function,
  // and returns the var_decl for it.
  tree struct_type_decl = gg_start_building_a_struct(type_name, current_function->function_decl);

  va_list params;
  va_start(params, count);

  gg_get_struct_type_decl(struct_type_decl, count, params);

  va_end(params);
  // We now have a complete struct_type_decl, whose TREE_TYPE is the
  // the type we need when declaring it.

  // And with that done, we can actually define the storage:
  tree var_decl = gg_define_variable( TREE_TYPE(struct_type_decl),
                                      var_name,
                                      vs_static);
  return var_decl;
  }

tree
gg_struct_field_ref(const tree base, const char *field)
  {
  tree retval;

  tree type = TREE_TYPE(base);
  if( POINTER_TYPE_P (type) )
    {
    tree pointer_type = TREE_TYPE(base);
    tree base_pointer_type = TREE_TYPE(pointer_type);
    // We need a COMPONENT_REF which is an INDIRECT_REF to a FIELD_DECL
    tree field_decl = gg_find_field_in_struct(base, field);
    tree indirect_ref = build1(INDIRECT_REF, base_pointer_type, base);
    retval = build3(COMPONENT_REF,
                    TREE_TYPE(field_decl),
                    indirect_ref,
                    field_decl,
                    NULL_TREE);
    }
  else
    {
    // It's not a pointer, so presumably it's a structure
    tree field_decl = gg_find_field_in_struct(base, field);
    retval = build3(COMPONENT_REF,
                    TREE_TYPE(field_decl),
                    base,
                    field_decl,
                    NULL_TREE);
    }
  return retval;
  }

tree
gg_assign_to_structure(tree var_decl_struct, const char *field, const tree source)
  {
  // The C equivalent:  "struct.field = source"
  tree component_ref = gg_struct_field_ref(var_decl_struct,field);
  gg_assign(component_ref,source);
  return component_ref;
  }

tree
gg_assign_to_structure(tree var_decl_struct, const char *field, int N)
  {
  // The C equivalent:  "struct.field = N"
  tree component_ref = gg_struct_field_ref(var_decl_struct,field);
  gg_assign(component_ref,build_int_cst(integer_type_node, N));
  return component_ref;
  }

static tree
gg_create_assembler_name(const char *cobol_name)
  {
  char *psz = cobol_name_mangler(cobol_name);
  tree retval = get_identifier(psz);
  free(psz);
  return retval;
  }

static char *
gg_unique_in_function(const char *var_name, gg_variable_scope_t vs_scope)
  {
  char *retval = (char *)xmalloc(strlen(var_name)+32);
  if( (vs_scope == vs_stack || vs_scope == vs_static) )
    {
    sprintf(retval, "%s.%ld", var_name, current_function->program_id_number);
    }
  else
    {
    strcpy(retval, var_name);
    }
  return retval;
  }

tree
gg_declare_variable(tree type_decl,
                    const char *name,
                    tree initial_value,
                    gg_variable_scope_t vs_scope,
                    bool *already_defined)
  {
  // The C/C++ language provides the concept of a *declaration*, which is a
  // prototype for a variable or function.  "extern int global_var" is a
  // declaration.  Declarations let the compiler know what kind of variable it
  // is looking for so that it can know what to do with it when it is found.
  //
  // A *definition* causes the assembler to actually create data storage for
  // the specified var_decl.
  //
  // Be it hereby known that the various attributes associated with a var_decl,
  // things like TREE_PUBLIC and TREE_STATIC and TREE_CONST seem to line up with
  // their meanings in the C language.  But I haven't investigated it enough to
  // be completely sure about that.  A hard look at gcc/tree.h is on my list of
  // homework assignments.  In the meantime, I continue to learn by compiling
  // C programs with the fdump-generic-nodes option, and copying them as
  // necessary to accomplish specific tasks.
  //
  // Specifically, this routine creates and returns a VAR_DECL, which is the
  // prototype.
  //
  // The gg_define_variable() routines take a VAR_DECL and create a DECL_EXPR
  // node from it.  When that DECL_EXPR is appended to the statement list, it
  // causes the storage to be allocated.

  // It is routine to let the compiler assign names to stack variables.  The
  // assembly code doesn't use names for variables on the stack; they are
  // referenced by offsets to the base pointer.  But static variables have to
  // have names, and there are places in my code generation -- Lord only knows
  // why -- where I didn't give the variables explicit names.  We remedy that
  // here:

  static std::map<std::string, tree>seen;

  tree var_name = NULL_TREE;
  tree var_decl;
  // Assume that for an external reference we know what we want:
  char *unique_name = NULL;
  if( name )
    {
    // We were provided a name
    unique_name = gg_unique_in_function(name, vs_scope);
    var_name = get_identifier(unique_name);
    std::map<std::string, tree>::const_iterator it = seen.find(unique_name);
    if( it != seen.end() )
      {
      // We've seen this one before
      var_decl = it->second;
      if( already_defined )
        {
        *already_defined = true;
        }
      }
    else
      {
      var_decl = build_decl(UNKNOWN_LOCATION,
                            VAR_DECL,
                            var_name,
                            type_decl);
      }
    }
  else
    {
    // We were not provided a name, so we have to create one.
    if( vs_scope == vs_static  )
      {
      // static variables have to have names:
      static int counter = 1;
      char ach[32];
      sprintf(ach, "__unnamed_static_variable_%d", counter++);
      var_name = get_identifier(ach);
      }
    var_decl = build_decl(UNKNOWN_LOCATION,
                          VAR_DECL,
                          var_name,
                          type_decl);
    }
  switch(vs_scope)
    {
    case vs_stack:
      // This is a stack variable
      DECL_CONTEXT(var_decl) = current_function->function_decl;
      break;
    case vs_static:
      // This is a function-level static variable
      DECL_CONTEXT(var_decl) = current_function->function_decl;
      TREE_STATIC(var_decl) = 1;
      break;
    case vs_file_static:
      // File static variables have translation_unit_scope.  I have chosen to
      // provide access to them through a map; see gg_trans_unit_var_decl();
      // TREE_STATIC seems to imply const.
      DECL_CONTEXT (var_decl) = gg_trans_unit.trans_unit_decl;
      TREE_STATIC(var_decl) = 1;
      break;
    case vs_file:
      // File variables have translation_unit_scope.
      // When TREE_STATIC is on, they seem to get put into the .text section
      DECL_CONTEXT (var_decl) = gg_trans_unit.trans_unit_decl;
      break;
    case vs_external:
      // This is for defining variables with global scope
      DECL_CONTEXT (var_decl) = gg_trans_unit.trans_unit_decl;
      TREE_USED(var_decl)   = 1;
      TREE_STATIC(var_decl) = 1;
      TREE_PUBLIC(var_decl) = 1;
      seen[unique_name] = var_decl;
      break;
    case vs_external_reference:
      // This is for referencing variables defined elsewhere
      DECL_CONTEXT (var_decl) = gg_trans_unit.trans_unit_decl;
      TREE_USED(var_decl)   = 1;
      DECL_EXTERNAL (var_decl) = 1;
      TREE_PUBLIC(var_decl) = 1;
      break;
    }
  DECL_INITIAL(var_decl) = initial_value;
  if( unique_name )
    {
    free(unique_name);
    }
  return var_decl;
  }

tree
gg_define_from_declaration(tree var_decl)
  {
  // Append the var_decl to either the chain for the current function or for
  // the translation_unit, depending on the var_decl's context:
  gg_append_var_decl(var_decl);

  if( !SCOPE_FILE_SCOPE_P(DECL_CONTEXT(var_decl)) )
    {
    // Having made sure the chain of variable declarations is nicely started,
    // it's time to actually define the storage with a decl_expression:
    tree stmt = build1_loc (location_from_lineno(),
                            DECL_EXPR,
                            TREE_TYPE(var_decl),
                            var_decl);
    gg_append_statement(stmt);
    }

  // And we are done.  That variable is now available for computation.
  return var_decl;
  }

tree
gg_define_variable(tree type_decl)
  {
  tree var_decl = gg_declare_variable(type_decl);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_variable(tree type_decl, tree initial_value)
  {
  tree var_decl = gg_declare_variable(type_decl,
                                      NULL,
                                      gg_cast(type_decl, initial_value),
                                      vs_stack);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_variable(tree type_decl, gg_variable_scope_t vs_scope)
  {
  tree var_decl = gg_declare_variable(type_decl, NULL, NULL_TREE, vs_scope);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_variable( tree type_decl,
                    const char *var_name,
                    gg_variable_scope_t vs_scope,
                    tree initial_value)
  {
  tree var_decl = gg_declare_variable(type_decl, var_name, initial_value, vs_scope);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_variable(tree type_decl, const char *name, gg_variable_scope_t vs_scope)
  {
  bool already_defined = false;
  tree var_decl = gg_declare_variable(type_decl, name, NULL_TREE, vs_scope, &already_defined);
  if( !already_defined )
    {
    gg_define_from_declaration(var_decl);
    }
  return var_decl;
  }

tree
gg_define_bool()
  {
  tree var_decl = gg_declare_variable(BOOL);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char()
  {
  // The nearest C equivalent: "char name;", but this one is given a
  // compiler-assigned name.
  // Beware:  This is the "implementation specific" version of char, which
  // in GENERIC seems to be signed on Windows/Linux Intel machines.  But we
  // need to be careful if we use an 8-bit type for numerical calculation.
  tree var_decl = gg_declare_variable(CHAR);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char(const char *variable_name)
  {
  // The C equivalent: "char name;"
  // Beware:  This is the "implementation specific" version of char, which
  // in GENERIC seems to be signed on Windows/Linux Intel machines.  But we
  // need to be careful if we use an 8-bit type for numerical calculation.
  tree var_decl = gg_declare_variable(CHAR, variable_name);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char(const char *variable_name, tree ch)
  {
  tree var_decl = gg_declare_variable(CHAR, variable_name, ch);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char(const char *variable_name, int ch)
  {
  return gg_define_char(variable_name, char_nodes[ch&0xFF]);
  }

tree
gg_define_uchar()
  {
  // The C equivalent: "char name;"
  // Beware:  This is the "implementation specific" version of char, which
  // in GENERIC seems to be signed on Windows/Linux Intel machines.  But we
  // need to be careful if we use an 8-bit type for numerical calculation.
  return gg_define_variable(UCHAR);
  }

tree
gg_define_uchar(const char *variable_name)
  {
  // The C equivalent: "char name;"
  // Beware:  This is the "implementation specific" version of char, which
  // in GENERIC seems to be signed on Windows/Linux Intel machines.  But we
  // need to be careful if we use an 8-bit type for numerical calculation.
  return gg_define_variable(UCHAR, variable_name);
  }

tree
gg_define_uchar(const char *variable_name, tree ch)
  {
  tree var_decl = gg_declare_variable(UCHAR, variable_name, ch);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar(const char *variable_name, int ch)
  {
  return gg_define_char(variable_name, char_nodes[ch&0xFF]);
  }

tree
gg_define_int()
  {
  tree var_decl = gg_declare_variable(INT);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int(int N)
  {
  tree var_decl = gg_declare_variable(INT, NULL, build_int_cst_type(INT, N));
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int(const char *variable_name)
  {
  tree var_decl = gg_declare_variable(INT, variable_name);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int(const char *variable_name, tree N)
  {
  tree var_decl = gg_declare_variable(INT, variable_name, N);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int(const char *variable_name, int N)
  {
  tree var_decl = gg_declare_variable(INT, variable_name, build_int_cst_type(INT, N));
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_size_t()
  {
  tree var_decl = gg_declare_variable(SIZE_T);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_size_t(const char *variable_name)
  {
  tree var_decl = gg_declare_variable(SIZE_T, variable_name);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_size_t(tree N)
  {
  tree retval = gg_define_variable(SIZE_T);
  gg_assign(retval, N);
  return retval;
  }

tree
gg_define_size_t(size_t N)
  {
  tree var_decl = gg_declare_variable(SIZE_T, NULL, build_int_cst_type(SIZE_T, N));
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_size_t(const char *variable_name, tree N)
  {
  tree var_decl = gg_declare_variable(SIZE_T, variable_name, N);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_size_t(const char *variable_name, size_t N)
  {
  tree var_decl = gg_declare_variable(SIZE_T, variable_name, build_int_cst_type(SIZE_T, N));
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int128()
  {
  // The C equivalent: "INT128 <compiler_name>;"
  return gg_define_variable(INT128);
  }

tree
gg_define_int128(const char *variable_name)
  {
  // The C equivalent: "INT128 name;"
  return gg_define_variable(INT128, variable_name);
  }

tree
gg_define_int128(const char *variable_name, tree N)
  {
  // The C equivalent: "INT128 name = N"
  tree var_decl = gg_declare_variable(INT128, variable_name, N);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_int128(const char *variable_name, int N)
  {
  // The C equivalent: "INT128 name = N"
  tree var_decl = gg_define_int128(variable_name, build_int_cst_type(INT128, N));
  return var_decl;
  }

tree
gg_define_char_star()
  {
  // The C equivalent: "char *name;"
  return gg_define_variable(CHAR_P);
  }

tree
gg_define_char_star(const char *variable_name)
  {
  return gg_define_variable(CHAR_P, variable_name);
  }

tree
gg_define_char_star(const char *variable_name, gg_variable_scope_t scope)
  {
  tree var_decl = gg_declare_variable(CHAR_P, variable_name, NULL_TREE, scope);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char_star(tree var)
  {
  tree var_decl = gg_declare_variable(CHAR_P, NULL, var);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_char_star(const char *variable_name, tree var)
  {
  tree var_decl = gg_declare_variable(CHAR_P, variable_name, var);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar_star()
  {
  tree var_decl = gg_declare_variable(UCHAR_P);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar_star(const char *variable_name)
  {
  tree var_decl = gg_declare_variable(UCHAR_P, variable_name);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar_star(const char *variable_name, gg_variable_scope_t scope)
  {
  tree var_decl = gg_declare_variable(UCHAR_P, variable_name, NULL_TREE, scope);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar_star(tree var)
  {
  tree var_decl = gg_declare_variable(UCHAR_P, NULL, var);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_uchar_star(const char *variable_name, tree var)
  {
  tree var_decl = gg_declare_variable(UCHAR_P, variable_name, var);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_void_star()
  {
  tree var_decl = gg_declare_variable(VOID_P);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_void_star(const char *variable_name)
  {
  tree var_decl = gg_declare_variable(VOID_P, variable_name);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_void_star(const char *variable_name, tree var)
  {
  tree var_decl = gg_declare_variable(VOID_P, variable_name, var);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_void_star(const char *variable_name, gg_variable_scope_t scope)
  {
  tree var_decl = gg_declare_variable(VOID_P, variable_name, NULL_TREE, scope);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

tree
gg_define_longdouble()
  {
  tree var_decl = gg_declare_variable(LONGDOUBLE);
  gg_define_from_declaration(var_decl);
  return var_decl;
  }

extern tree
gg_define_array(tree type_decl, size_t size)
  {
  tree array_type = build_array_type_nelts(type_decl, size);
  return gg_define_variable(array_type);
  }

extern tree
gg_define_array(tree type_decl, const char *name, size_t size)
  {
  tree array_type = build_array_type_nelts(type_decl, size);
  return gg_define_variable(array_type, name);
  }

extern tree
gg_define_array(tree type_decl, size_t size, gg_variable_scope_t scope)
  {
  tree array_type = build_array_type_nelts(type_decl, size);
  return gg_define_variable(array_type, scope);
  }

extern tree
gg_define_array(tree type_decl, const char *name, size_t size, gg_variable_scope_t scope)
  {
  tree array_type = build_array_type_nelts(type_decl, size);
  return gg_define_variable(array_type, name, scope);
  }

tree
gg_get_address_of(const tree var_decl)
  {
  // Returns an ADDR_EXPR which points to var_decl.
  // The C equivalent is &variable
  // We need to be able to use this guy's address directly:

  // In order to do that, this fellow's "addressable" bit has to be on, otherwise
  // the GIMPLE reducer creates a temporary variable, sets its value to var_decl's,
  // and returns the pointer to the temp.  I suppose this has something to do with
  // pass by reference and pass by value, but it makes my head hurt, and, frankly,
  // I'll take the dangerous road.

  TREE_ADDRESSABLE(var_decl) = 1;
  TREE_USED(var_decl) = 1;
  return build1(  ADDR_EXPR,
                  build_pointer_type (TREE_TYPE(var_decl)),
                  var_decl);
  }

tree
gg_get_indirect_reference(tree pointer, tree offset)
  {
  // The C equivalent: auto pointer[offset];

  // the returned indirect reference has the same type as
  // what pointer points to.  If pointer is a char *, then the returned
  // value has type char.  If pointer is an int *, then the returned
  // value has type int.

  // We also want the offset to operate the same way it does in C, so we
  // are going to find the size of the objects the pointer points to, and
  // multiply the offset by that size:

  tree pointer_type = TREE_TYPE(pointer);
  tree element_type = TREE_TYPE(pointer_type);

  tree indirect_reference;
  if( offset )
    {
    // We can now start building our little shrub:
    tree distance = build2( MULT_EXPR,
                            SIZE_T,
                            gg_cast(sizetype, offset),
                            TYPE_SIZE_UNIT(element_type));

    // Next, we build the pointer_plus_expr:
    tree pointer_plus_expr = build2(POINTER_PLUS_EXPR,
                                    pointer_type,
                                    pointer,
                                    distance);

    // With that in hand, we can build the indirect_reference:
    indirect_reference = build1(INDIRECT_REF, element_type, pointer_plus_expr);
    }
  else
    {
    indirect_reference = build1(INDIRECT_REF, element_type, pointer);
    }

  return indirect_reference;
  }

tree
gg_indirect(tree pointer, tree byte_offset)
  {
  // Unlike gg_get_indirect_reference, which multiplies the offset by the
  // size of the type pointed to by pointer, this routine simply adds the offset
  // to the pointer.
  tree pointer_type = TREE_TYPE(pointer);
  tree element_type = TREE_TYPE(pointer_type);

  tree retval;
  if( byte_offset == NULL_TREE )
    {
    retval = build1(INDIRECT_REF, element_type, pointer);
    }
  else
    {
    tree pointer_plus_expr = build2(POINTER_PLUS_EXPR,
                                    pointer_type,
                                    pointer,
                                    gg_cast(SIZE_T, byte_offset));
    retval = build1(INDIRECT_REF, element_type, pointer_plus_expr);
    }

  return retval;
  }

tree
gg_array_value(tree pointer, tree offset)
  {
  // We arrange the function so that it can work on either an ARRAY_TYPE
  // or a pointer type
  tree pointer_type = TREE_TYPE(pointer);
  tree element_type = TREE_TYPE(pointer_type);
  if(POINTER_TYPE_P(pointer_type))
    {
    // It is a pointer
    tree retval = gg_get_indirect_reference(pointer, offset);
    return retval;
    }
  else
    {
    return build4(ARRAY_REF,
                  element_type,
                  pointer,
                  offset,
                  NULL_TREE,
                  NULL_TREE);
    }
  }

tree
gg_array_value(tree pointer, int N)
  {
  return gg_array_value(pointer, build_int_cst(INT, N));
  }

void
gg_increment(tree var)
  {
  tree var_type = TREE_TYPE(var);
  gg_assign(var, gg_add(var, build_int_cst_type(var_type, 1)));
  }

void
gg_decrement(tree var)
  {
  tree var_type = TREE_TYPE(var);
  gg_assign(var,
            gg_cast(var_type,
                    gg_subtract(var,
                                build_int_cst_type(var_type, 1))));
  }

tree
gg_negate(tree var)
  {
  return build1(NEGATE_EXPR, TREE_TYPE(var), var);
  }

tree
gg_bitwise_not(tree var)
  {
  return build1(BIT_NOT_EXPR, TREE_TYPE(var), var);
  }

tree
gg_abs(tree var)
  {
  return build1(ABS_EXPR, TREE_TYPE(var), var);
  }

static tree
gg_get_larger_type(tree A, tree B)
  {
  tree larger = TREE_TYPE(B);
  if(    TREE_INT_CST_LOW(TYPE_SIZE(TREE_TYPE(A)))
       > TREE_INT_CST_LOW(TYPE_SIZE(TREE_TYPE(B))) )
    {
    larger = TREE_TYPE(A);
    }
  return larger;
  }

tree
gg_add(tree addend1, tree addend2)
  {
  tree retval;
  if( POINTER_TYPE_P(TREE_TYPE(addend1)) )
    {
    //  operand1 is a pointer.
    //  Make this work like C pointer arithmetic.  We'll find the
    //  size of the things that pointer points to, and multiply accordingly
    tree pointer_type = TREE_TYPE(addend1);
    tree pointer_type_type = TREE_TYPE(pointer_type);
    tree bytes_per_element = TYPE_SIZE_UNIT(pointer_type_type);

    tree op2 = gg_cast(SIZE_T, gg_multiply(addend2, bytes_per_element));
    retval = build2(POINTER_PLUS_EXPR,
                    TREE_TYPE(addend1),
                    addend1,
                    op2);
    }
  else
    {
    // Ordinary addition.  Scale both operands to match the larger
    // type of the two operands.
    tree larger_type = gg_get_larger_type(addend1, addend2);
    retval = build2( PLUS_EXPR,
                     larger_type,
                     gg_cast(larger_type, addend1),
                     gg_cast(larger_type, addend2));
    }
  return retval;
  }

tree
gg_subtract(tree A, tree B)
  {
  // We are doing A - B, instead.

  if( POINTER_TYPE_P(TREE_TYPE(A)) && INTEGRAL_TYPE_P(TREE_TYPE(B)) )
    {
    // We are subtracting an integer from a pointer.  That's handled
    // in gg_add, by converting the integer, possibly signed, to
    // an unsigned huge number.
    return gg_add(A, gg_negate(B));
    }

  if( POINTER_TYPE_P(TREE_TYPE(A)) && POINTER_TYPE_P(TREE_TYPE(A)) )
    {
    // We are subtracting two pointers, yielding a signed size_t
    return build2(POINTER_DIFF_EXPR, SSIZE_T, A, B);
    }

  // This is an ordinary subtraction. Scale everything to the larger_type
  // of the two operands.
  tree larger_type = gg_get_larger_type(A, B);
  tree stmt = build2( MINUS_EXPR,
                      larger_type,
                      gg_cast(larger_type, A),
                      gg_cast(larger_type, B) );
  return stmt;
  }

tree
gg_multiply(tree A, tree B)
  {
  // We will return the product of A and B, adjusting to
  // whichever is larger:
  tree larger_type = gg_get_larger_type(A, B);
  return build2( MULT_EXPR, larger_type, gg_cast(larger_type, A), gg_cast(larger_type, B) );
  }

tree
gg_real_divide(tree A, tree B)
  {
  // This floating point division:
  tree larger_type = gg_get_larger_type(A, B);
  return build2( RDIV_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_divide(tree A, tree B)
  {
  // This is the equivalent of C integer divide
  tree larger_type = gg_get_larger_type(A, B);
  return build2( TRUNC_DIV_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_mod(tree A, tree B)
  {
  // This is the equivalent of C  A % B
  tree larger_type = gg_get_larger_type(A, B);
  return build2( TRUNC_MOD_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_lshift(tree A, tree B)
  {
  // Equivalent of A << B;
  return build2( LSHIFT_EXPR, TREE_TYPE(A), A, B );
  }

tree
gg_rshift(tree A, tree B)
  {
  // Equivalent of A >> B;
  return build2( RSHIFT_EXPR, TREE_TYPE(A), A, B );
  }

tree
gg_bitwise_or(tree A, tree B)
  {
  // This is C equivalent to A | B
  tree larger_type = gg_get_larger_type(A, B);
  return build2( BIT_IOR_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_bitwise_xor(tree A, tree B)
  {
  // This is C equivalent to A ^ B
  tree larger_type = gg_get_larger_type(A, B);
  return build2( BIT_XOR_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_bitwise_and(tree A, tree B)
  {
  // This is C equivalent to A & B
  tree larger_type = gg_get_larger_type(A, B);
  return build2( BIT_AND_EXPR, larger_type, gg_cast(larger_type,A), gg_cast(larger_type,B));
  }

tree
gg_build_relational_expression(tree operand_a,
                               enum relop_t op,
                               tree operand_b)
  {
  tree_code compare = EQ_EXPR;  // Assuage the compiler
  switch(op)
    {
    case eq_op:
      compare = EQ_EXPR;
      break;
    case ne_op:
      compare = NE_EXPR;
      break;
    case lt_op:
      compare = LT_EXPR;
      break;
    case gt_op:
      compare = GT_EXPR;
      break;
    case ge_op:
      compare = GE_EXPR;
      break;
    case le_op:
      compare = LE_EXPR;
      break;
    }
  tree relational_expression = build2_loc(location_from_lineno(),
                                          compare,
                                          boolean_type_node,
                                          operand_a,
                                          operand_b);
  return relational_expression;
  }

tree
gg_build_logical_expression(tree operand_a,
                            enum logop_t op,
                            tree operand_b)
  {
  tree logical_expression = NULL_TREE;
  tree_code logical_op;
  switch(op)
    {
    case and_op:
      logical_op = TRUTH_ANDIF_EXPR;
      logical_expression = build2(logical_op,
                                  boolean_type_node,
                                  operand_a,
                                  operand_b);
      break;

    case or_op:
      logical_op = TRUTH_ORIF_EXPR;
      logical_expression = build2(logical_op,
                                  boolean_type_node,
                                  operand_a,
                                  operand_b);
      break;

    case not_op:
      logical_op = TRUTH_NOT_EXPR;
      logical_expression = build1(logical_op,
                                  boolean_type_node,
                                  operand_b);
      break;

    case xor_op:
      logical_op = TRUTH_XOR_EXPR;
      logical_expression = build2(logical_op,
                                  boolean_type_node,
                                  operand_a,
                                  operand_b);
      break;

    case xnor_op:
    case true_op:
    case false_op:
      // This is handled elsewhere
      break;
    }
  return logical_expression;
  }

void
gg_create_goto_pair(tree *goto_expr, tree *label_expr, tree *label_addr, const char *name)
  {
  // We are going to create a pair of expressions for our
  // caller.  They are a matched set of goto/label expressions,
  // to be included in a statement list
  tree label_decl = build_decl(   UNKNOWN_LOCATION,
                                  LABEL_DECL,
                                  gg_create_assembler_name(name),
                                  void_type_node);
  DECL_CONTEXT(label_decl) = current_function->function_decl;
  TREE_USED(label_decl) = 1;

  *goto_expr  = build1(GOTO_EXPR, void_type_node, label_decl);
  *label_expr = build1(LABEL_EXPR, void_type_node, label_decl);
  *label_addr = gg_get_address_of(label_decl);
  }

void
gg_create_goto_pair(tree *goto_expr, tree *label_expr, tree *label_addr)
  {
  // We are going to create a pair of expressions for our
  // caller.  They are a matched set of goto/label expressions,
  // to be included in a statement list
  tree label_decl = build_decl(   UNKNOWN_LOCATION,
                                  LABEL_DECL,
                                  NULL_TREE,
                                  void_type_node);
  DECL_CONTEXT(label_decl) = current_function->function_decl;
  TREE_USED(label_decl) = 1;

  *goto_expr  = build1(GOTO_EXPR, void_type_node, label_decl);
  *label_expr = build1(LABEL_EXPR, void_type_node, label_decl);
  *label_addr = gg_get_address_of(label_decl);
  }

void
gg_create_goto_pair(tree *goto_expr,
                    tree *label_expr,
                    tree *label_addr,
                    tree *label_decl)
  {
  // We are going to create a pair of expressions for our
  // caller.  They are a matched set of goto/label expressions,
  // to be included in a statement list
  *label_decl = build_decl( UNKNOWN_LOCATION,
                            LABEL_DECL,
                            NULL_TREE,
                            void_type_node);
  DECL_CONTEXT(*label_decl) = current_function->function_decl;
  TREE_USED(*label_decl) = 1;

  *goto_expr  = build1(GOTO_EXPR, void_type_node, *label_decl);
  *label_expr = build1(LABEL_EXPR, void_type_node, *label_decl);
  *label_addr = gg_get_address_of(*label_decl);
  }

void
gg_goto_label_decl(tree label_decl)
  {
  tree goto_expr  = build1_loc( location_from_lineno(),
                                GOTO_EXPR,
                                void_type_node,
                                label_decl);
  gg_append_statement(goto_expr);
  }

void
gg_create_goto_pair(tree *goto_expr, tree *label_expr)
  {
  // We are going to create a pair of expressions for our
  // caller.  They are a matched set of goto/label expressions,
  // to be included in a statement list
  tree label_decl = build_decl(   UNKNOWN_LOCATION,
                                  LABEL_DECL,
                                  NULL_TREE,
                                  void_type_node);
  DECL_CONTEXT(label_decl) = current_function->function_decl;
  TREE_USED(label_decl) = 1;

  *goto_expr = build1(GOTO_EXPR, void_type_node, label_decl);
  *label_expr = build1(LABEL_EXPR, void_type_node, label_decl);
  }

void
gg_create_goto_pair(tree *goto_expr, tree *label_expr, const char *name)
  {
  // We are going to create a pair of named expressions for our
  // caller.  They are a matched set of goto/label expressions,
  // to be included in a statement list
  tree label_decl = build_decl(   UNKNOWN_LOCATION,
                                  LABEL_DECL,
                                  gg_create_assembler_name(name),
                                  void_type_node);
  DECL_CONTEXT(label_decl) = current_function->function_decl;
  TREE_USED(label_decl) = 1;

  *goto_expr = build1(GOTO_EXPR, void_type_node, label_decl);
  *label_expr = build1(LABEL_EXPR, void_type_node, label_decl);
  }

// Used for implementing SECTIONS and PARAGRAPHS.  When you have a
// void *pointer = &&label, gg_goto is the same as
//  goto *pointer
void
gg_goto(tree var_decl_pointer)
  {
  tree go_to = build1_loc(location_from_lineno(),
                          GOTO_EXPR,
                          void_type_node,
                          var_decl_pointer);
  gg_append_statement(go_to);
  }

void
gg_while(   tree operand_a,
            enum relop_t op,
            tree operand_b)
  {
  /*
  See demonstration_while_if for the canonical demonstration

  You use it like this:

      WHILE
          ....
          WEND

  We do the C construct:

      while( a OP b )
          {
          <block>
          }

  like this:

      goto test
      top:
          <block>
      test:
          if( a OP b)
              goto top
          else
              goto leave:
      leave:

  */

  tree goto_top;
  tree label_top;

  tree goto_test;
  tree label_test;

  tree goto_leave;
  tree label_leave;

  gg_create_goto_pair(&goto_top,   &label_top);
  gg_create_goto_pair(&goto_test,  &label_test);
  gg_create_goto_pair(&goto_leave, &label_leave);

  tree statement_block = make_node(STATEMENT_LIST);
  TREE_TYPE(statement_block) = void_type_node;

  // During development, I tried appending a statement_list to a statement_list,
  // intending it to be collected together that way.  But it was too smart for me;
  // it just unwound the second list and tacked it onto the end of the first.

  // So I used a BIND_EXPR to collect them together.  This isn't a new context, so I don't
  // point operand[0] at a string of vars, nor operand[2] at a block.
  tree bind_expr = build3(   BIND_EXPR,
                             void_type_node,
                             NULL_TREE,
                             statement_block,
                             NULL_TREE);

  // With the pairs created and the bind_expr sorted out, we can now put
  // together our while construction:

  gg_append_statement(goto_test);
  gg_append_statement(label_top);
  gg_append_statement(bind_expr);
  gg_append_statement(label_test);
  IF( operand_a, op, operand_b )
  gg_append_statement(goto_top);
  ELSE
  gg_append_statement(goto_leave);
  ENDIF
  gg_append_statement(label_leave);

  // And here's the statement_list for the programmer to fill
  // and end with a WEND
  current_function->statement_list_stack.push_back(statement_block);
  }

void
gg_create_true_false_statement_lists(tree relational_expression)
  {
  // Create the two statement_lists for ifness, one for true and
  // the other for false.  Put them on the stack, ready for the first
  // pop on ELSE and the second pop on ENDIF:

  tree if_true_statement_list = make_node(STATEMENT_LIST);
  TREE_TYPE(if_true_statement_list) = void_type_node;
  tree if_false_statement_list = make_node(STATEMENT_LIST);
  TREE_TYPE(if_false_statement_list) = void_type_node;

  tree conditional = build3(  COND_EXPR,
                              boolean_type_node,
                              relational_expression,
                              if_true_statement_list,
                              if_false_statement_list);

  // We need to put our conditional onto the current_stack:
  gg_append_statement(conditional);

  // And with that done, we can push the FALSE and TRUE blocks
  // onto the stack in the correct order:
  current_function->statement_list_stack.push_back(if_false_statement_list);
  current_function->statement_list_stack.push_back(if_true_statement_list);
  }

void
gg_if(   tree operand_a,
         enum relop_t op,
         tree operand_b)
  {
  /*  Listen up, troops.  Here's how you use this constructor.

      You use it like this:

          IF( this, LT, that)
              ....
          ELSE
              ....
              ENDIF

      You *must* have all three: IF ELSE ENDIF, if you don't, the
      current_function->statement_list_stack gets all higgledepiggledy

      It is the C equivalent of

          if( a OP b )
              {
              <if_true_statement_list>
              }
          else
              {
              <if_false_statement_list>
              }

      This routine pushes the false_statement_list onto current_function->statement_list_stack,
      followed by the true_statement_list.

      You then generate statements for the TRUE block
      You then pop the current_function->statement_list_stack.
      Then you do the same for the FALSE block
      You then pop the current_function->statement_list_stack again.

      For the sake of readability, we define ELSE and ENDIF to do
      that popping.

      I don't plan on explaining this everywhere it's used.

      See demonstration_while_if for the canonical demonstration
      */

  if( TREE_TYPE(operand_a) != TREE_TYPE(operand_b) )
    {
    fprintf(stderr, "%s(): a and b have different TREE_TYPES\n", __func__);
    gcc_unreachable();
    }

  // Build the relational expression:
  tree relational_expression =
    gg_build_relational_expression(operand_a,
                                   op,
                                   operand_b);

  // And with that in hand, create the two statement lists, one for
  // true and one for false, and set up the stacks:
  gg_create_true_false_statement_lists(relational_expression);
  }

tree
gg_get_function_address(tree return_type, const char *funcname)
  {
  // This routine finds a function by name.  It calls build_fn_decl
  // with an empty array of varargs.  I haven't investigated all the
  // possibilities, but this returns an address expression for a function
  // that can be built with any argument[s].

  // There is no compile-time checking; if you specify disaster, then
  // disaster will be what you get.
  tree fndecl_type = build_varargs_function_type_array (return_type,
                     0,
                     NULL);
  tree function_decl = build_fn_decl (funcname, fndecl_type);
  DECL_EXTERNAL (function_decl) = 1;

  tree retval = build1(ADDR_EXPR, build_pointer_type (fndecl_type), function_decl);

  return retval;
  }

void
gg_printf(const char *format_string, ...)
  {
  // This allows you to use fprintf(stderr, ...) with a format string
  // and a list of arguments ending with a NULL

  // Use this for conveniently adding print statements into the generated
  // code, for run-time print-statement debugging.  gg_write is used for
  // actual program code.

  // Note that the return value from the printf() call is *not* available
  // to the caller.

  int nargs = 0;
  tree args[ARG_LIMIT];

  // Because this routine is intended for debugging, we are sending the
  // text to STDERR

  // Because we don't actually use stderr ourselves, we just pick it up as a
  // VOID_P and pass it along to fprintf()
  tree t_stderr = gg_declare_variable(VOID_P, "stderr",
                                      NULL_TREE,
                                      vs_external_reference);

  gg_push_context();

  args[nargs++] = t_stderr;
  args[nargs++] = build_string_literal(strlen(format_string)+1, format_string);

  va_list ap;
  va_start(ap, format_string);
  tree arg = va_arg(ap, tree);
  while(arg)
    {
    if(nargs >= ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### You *must* be joking!");
      gcc_unreachable();
      }

    if( TREE_CODE(arg) >= NUM_TREE_CODES)
      {
      // Warning:  This test is not completely reliable, because a garbage
      // byte could have a valid TREE_CODE.  But it does help.
      yywarn("You nitwit!");
      yywarn("You forgot to put a NULL_TREE at the end of a "
                  "gg_printf() again!");
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      gcc_unreachable();
      }

    args[nargs++] = arg;
    arg = va_arg(ap, tree);
    }
  va_end (ap);

  static tree function = NULL_TREE;
  if( !function )
    {
    function = gg_get_function_address(INT, "fprintf");
    }

  tree stmt = build_call_array_loc (location_from_lineno(),
                                    INT,
                                    function,
                                    nargs,
                                    args);
  gg_append_statement(stmt);

  gg_pop_context();
  }

tree
gg_fprintf(tree fd, int nargs, const char *format_string, ...)
  {
  tree retval = gg_define_int();
  gg_push_context();
  tree buffer = gg_define_char_star();
  gg_assign(buffer, gg_cast(CHAR_P, gg_malloc(1024)));

  tree args[ARG_LIMIT];

  // Set up a call to sprintf:
  int argc = 0;
  args[argc++] = buffer;
  args[argc++] = build_string_literal(strlen(format_string)+1, format_string);

  va_list ap;
  va_start(ap, format_string);
  tree arg = va_arg(ap, tree);
  int narg = 0;
  while(narg++ < nargs)
    {
    if(argc >= ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### You *must* be joking!");
      gcc_unreachable();
      }

    args[argc++] = arg;
    arg = va_arg(ap, tree);
    }
  va_end (ap);

  static tree function = NULL_TREE;

  if( !function )
    {
    function = gg_get_function_address(INT, "sprintf");
    }

  tree stmt = build_call_array_loc (location_from_lineno(),
                                    INT,
                                    function,
                                    argc,
                                    args);
  gg_assign(retval, stmt);
  gg_write(fd, buffer, gg_strlen(buffer));

  gg_free(buffer);
  gg_pop_context();
  return retval;
  }

tree
gg_read(tree fd, tree buf, tree count)
  {
  // The C equivalent: "read(fd, buf, count)"

  // Because the caller might need the ssize_t return value, this routine
  // returns the statement_decl for the call.  It is used this way:

  // tree num_chars = gg_define_int("_num_chars");
  // gg_assign(num_chars, gg_read(fd, buf, count));

  return gg_call_expr(SSIZE_T,
                      "read",
                      fd,
                      buf,
                      count,
                      NULL_TREE);
  }

void
gg_write(tree fd, tree buf, tree count)
  {
  gg_call(SSIZE_T,
          "write",
          fd,
          buf,
          count,
          NULL_TREE);
  }

void
gg_memset(tree dest, const tree value, tree size)
  {
  tree the_call =
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_MEMSET),
                          3,
                          dest,
                          value,
                          size);
  gg_append_statement(the_call);
  }

tree
gg_memchr(tree buf, tree ch, tree length)
  {
  tree the_call = fold_convert(
      pvoid_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_MEMCHR),
                          3,
                          buf,
                          ch,
                          length));
  return the_call;
  }

/*  Built-in call to memcpy() */

void
gg_memcpy(tree dest, const tree src, tree size)
  {
  tree the_call = build_call_expr_loc(
        location_from_lineno(),
        builtin_decl_explicit (BUILT_IN_MEMCPY),
        3,
        dest,
        src,
        size);
  gg_append_statement(the_call);
  }

/*  Built-in call to memmove() */

void
gg_memmove(tree dest, const tree src, tree size)
  {
  tree the_call = build_call_expr_loc(
        location_from_lineno(),
        builtin_decl_explicit (BUILT_IN_MEMMOVE),
        3,
        dest,
        src,
        size);
  gg_append_statement(the_call);
  }

tree
gg_memdup(tree data, tree length)
  {
  // Duplicates data; gg_free should eventually be called
  tree retval = gg_define_char_star();
  gg_assign(retval, gg_malloc(length));
  gg_memcpy(retval, data, length);
  return retval;
  }

tree
gg_memdup(tree data, size_t length)
  {
  // Duplicates data; gg_free should eventually be called
  tree retval = gg_define_char_star();
  gg_assign(retval, gg_malloc(length));
  gg_memcpy(retval, data, build_int_cst_type(SIZE_T, length));
  return retval;
  }

void
gg_strcpy(tree dest, tree src)
  {
  tree the_call =
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_STRCPY),
                          2,
                          dest,
                          src);
  gg_append_statement(the_call);
  }

tree
gg_strcmp(tree A, tree B)
  {
  tree the_call = fold_convert(
      integer_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_STRCMP),
                          2,
                          A,
                          B));
  return the_call;
  }

tree
gg_open(tree char_star_A, tree int_B)
  {
  return gg_call_expr(INT,
                      "open",
                      char_star_A,
                      int_B,
                      NULL_TREE);
  }

tree
gg_close(tree int_A)
  {
  return gg_call_expr(INT,
                      "close",
                      int_A,
                      NULL_TREE);
  }

tree
gg_strncmp(tree char_star_A, tree char_star_B, tree size_t_N)
  {
  tree the_call = fold_convert(
      integer_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_STRNCMP),
                          3,
                          char_star_A,
                          char_star_B,
                          size_t_N));
  return the_call;
  }

void
gg_return(tree operand)
  {
  tree stmt;

  if( !gg_trans_unit.function_stack.size() )
    {
    // I put this in to cope with the problem of two END PROGRAM statements, which
    // should be a syntax error but, as of 2021-02-24, is ignored by GnuCOBOL and
    // by our parser.
    return ;
    }

  // We have to pop ourselves off of the module_name_stack:
  gg_call(VOID,
          "__gg__module_name_pop",
          NULL_TREE);

  if( !operand || !DECL_RESULT(current_function->function_decl) )
    {
    // When there is no operand, or if the function result is void, then
    // we just generate a return_expr.
    stmt = build1_loc(location_from_lineno(), RETURN_EXPR, void_type_node, NULL_TREE);
    }
  else
    {
    // Life is a wee bit more complicated, because we want to return the operand
    tree function_type = TREE_TYPE(DECL_RESULT(current_function->function_decl));
    tree modify = build2(   MODIFY_EXPR,
                            function_type,
                            DECL_RESULT(current_function->function_decl),
                            gg_cast(function_type, operand));
    stmt = build1_loc(location_from_lineno(), RETURN_EXPR, void_type_node, modify);
    }
  gg_append_statement(stmt);
  }

void
chain_parameter_to_function(tree function_decl, const tree param_type,  const char *name)
  {
  tree parm = build_decl (location_from_lineno(),
                          PARM_DECL,
                          get_identifier (name),
                          param_type);
  DECL_CONTEXT(parm) = function_decl;
  TREE_USED(parm) = 1;
  DECL_INITIAL(parm) = param_type;

  if( DECL_ARGUMENTS(function_decl) )
    {
    chainon(DECL_ARGUMENTS(function_decl),parm);
    }
  else
    {
    DECL_ARGUMENTS(function_decl) = parm;
    }
  }

void
gg_modify_function_type(tree function_decl, tree return_type)
  {
  tree fndecl_type = build_varargs_function_type_array( return_type,
                     0,     // No parameters yet
                     NULL); // And, hence, no types
  TREE_TYPE(function_decl)  = fndecl_type;
  tree resdecl = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resdecl) = function_decl;
  DECL_RESULT (function_decl) = resdecl;
  }

tree
gg_define_function_with_no_parameters(tree return_type,
                                      const char *funcname,
                                      const char *unmangled_name)
  {
  // This routine builds a function_decl, puts it on the stack, and
  // gives it a context.

  // At this time we don't know how many parameters this function expects, so
  // we set things up and we'll tack on the parameters later.

  // Create the FUNCTION_TYPE for that array:
  // int nparams = 1;
  // tree types[1] = {VOID_P};
  // const char *names[1] = {"_p1"};

  // tree fndecl_type = build_varargs_function_type_array( return_type,
  // nparams,
  // types);

  tree fndecl_type = build_varargs_function_type_array( return_type,
                     0,     // No parameters yet
                     NULL); // And, hence, no types

  // Create the FUNCTION_DECL for that FUNCTION_TYPE
  tree function_decl = build_fn_decl (funcname, fndecl_type);

  // Some of this stuff is magical, and is based on compiling C programs
  // and just mimicking the results.
  TREE_ADDRESSABLE(function_decl) = 1;
  TREE_STATIC(function_decl) = 1;
  DECL_EXTERNAL (function_decl) = 0;
  DECL_PRESERVE_P (function_decl) = 0;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(function_decl) = 1;
  DECL_ARTIFICIAL(function_decl) = 0;
  TREE_NOTHROW(function_decl) = 0;
  TREE_USED(function_decl) = 1;

  // This code makes COBOL nested programs actual visible on the
  // source code "trans_unit_decl" level, but with non-public "static"
  // visibility.
  if( gg_trans_unit.function_stack.size() == 0 )
    {
    // gg_trans_unit.function_stack is empty, so our context is
    // the compilation module, and we need to be public:
    DECL_CONTEXT (function_decl) = gg_trans_unit.trans_unit_decl;
    TREE_PUBLIC(function_decl) = 1;
    }
  else
    {
    // The stack has something in it, so we are building a nested function.
    // Make the current function our context
    DECL_CONTEXT (function_decl) = gg_trans_unit.trans_unit_decl;
    TREE_PUBLIC(function_decl) = 0;

    // This function is file static, but nobody calls it, so without
    // intervention -O1+ optimizations will discard it.
    DECL_PRESERVE_P (function_decl) = 1;

    // Append this function to the list of functions and variables
    // associated with the computation module.
    gg_append_var_decl(function_decl);
    }

  // Establish the RESULT_DECL for the function:
  tree resdecl = build_decl (location_from_lineno(), RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resdecl) = function_decl;
  DECL_RESULT (function_decl) = resdecl;

  // The function_decl has a .function member, a pointer to struct_function.
  // This is quietly, almost invisibly, extremely important.  You need to
  // call this routine after DECL_RESULT has been established:

  allocate_struct_function(function_decl, false);

  struct gg_function_t new_function = {};
  new_function.context_count = 0;
  new_function.function_decl = function_decl;
  new_function.our_name = IDENTIFIER_POINTER(DECL_NAME(function_decl));
  new_function.our_unmangled_name = xstrdup(unmangled_name);
  new_function.function_address = gg_get_function_address(VOID, new_function.our_name);

  // Each program on the stack gets a unique identifier.  This is used, for
  // example, to make sure that static variables have unique names.
  static size_t program_id = 0;
  new_function.program_id_number = program_id++;

  // With everything established, put this function_decl on the stack
  gg_trans_unit.function_stack.push_back(new_function);

  // All we need is a context, and we are ready to go:
  gg_push_context();
  return function_decl;
  }

void
gg_tack_on_function_parameters(tree function_decl, ...)
  {
  int nparams = 0;

  tree types[ARG_LIMIT];
  const char *names[ARG_LIMIT];

  va_list params;
  va_start(params, function_decl);
  for(;;)
    {
    tree var_type = va_arg(params, tree);
    if( !var_type )
      {
      break;
      }

    if( TREE_CODE(var_type) >= NUM_TREE_CODES)
      {
      // Warning:  This test is not completely reliable, because a garbage
      // byte could have a valid TREE_CODE.  But it does help.
      yywarn("You nitwit!");
      yywarn("You forgot to put a NULL_TREE at the end of a "
                  "gg_define_function() again!");
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      gcc_unreachable();
      }

    const char *name = va_arg(params, const char *);

    types[nparams] = var_type;
    names[nparams] = name;
    nparams += 1;
    if(nparams > ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### %d parameters?  Really?  Are you insane?",ARG_LIMIT+1);
      gcc_unreachable();
      }
    }
  va_end(params);

  // Chain the names onto the variables list:
  for(int i=0; i<nparams; i++)
    {
    chain_parameter_to_function(function_decl, types[i], names[i]);
    }
  }

void
gg_define_function(tree return_type, const char *funcname, ...)
  {
  // This routine builds a function_decl, puts it on the stack, and
  // gives it a context.

  // After the funcname, we expect the formal parameters: pairs of types/names
  // terminated by a NULL_TREE

  int nparams = 0;

  tree types[ARG_LIMIT];
  const char *names[ARG_LIMIT];

  va_list params;
  va_start(params,funcname);
  for(;;)
    {
    tree var_type = va_arg(params, tree);
    if( !var_type )
      {
      break;
      }

    if( TREE_CODE(var_type) >= NUM_TREE_CODES)
      {
      // Warning:  This test is not completely reliable, because a garbage
      // byte could have a valid TREE_CODE.  But it does help.
      yywarn("You nitwit!");
      yywarn("You forgot to put a NULL_TREE at the end of a "
                  "gg_define_function() again!");
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      gcc_unreachable();
      }

    const char *name = va_arg(params, const char *);

    types[nparams] = var_type;
    names[nparams] = name;
    nparams += 1;
    if(nparams > ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### %d parameters?  Really?  Are you insane?",
                  ARG_LIMIT+1);
      gcc_unreachable();
      }
    }
  va_end(params);

  // Create the FUNCTION_TYPE for that array:
  tree fndecl_type = build_varargs_function_type_array( return_type,
                     nparams,
                     types);

  // Create the FUNCTION_DECL for that FUNCTION_TYPE
  tree function_decl = build_fn_decl (funcname, fndecl_type);

  // Some of this stuff is magical, and is based on compiling C programs
  // and just mimicking the results.
  TREE_ADDRESSABLE(function_decl) = 1;
  TREE_STATIC(function_decl) = 1;
  DECL_EXTERNAL (function_decl) = 0;
  DECL_PRESERVE_P (function_decl) = 0;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(function_decl) = 1;
  DECL_ARTIFICIAL(function_decl) = 0;
  TREE_NOTHROW(function_decl) = 0;
  TREE_USED(function_decl) = 1;

  // This code makes COBOL nested programs actual visible on the
  // source code "trans_unit_decl" level, but with non-public "static"
  // visibility.
  if( gg_trans_unit.function_stack.size() == 0 )
    {
    // gg_trans_unit.function_stack is empty, so our context is
    // the compilation module, and we need to be public:
    DECL_CONTEXT (function_decl) = gg_trans_unit.trans_unit_decl;
    TREE_PUBLIC(function_decl) = 1;
    }
  else
    {
    // The stack has something in it, so we are building a nested function.
    // Make the current function our context
    DECL_CONTEXT (function_decl) = gg_trans_unit.trans_unit_decl;

    // We need to make it public, because otherwise COBOL CALL "func"
    // won't be able to find it, because dlopen/dlsym won't find it.
    TREE_PUBLIC(function_decl) = 0;

    // Append this function to the list of functions and variables
    // associated with the computation module.
    gg_append_var_decl(function_decl);
    }

  // Chain the names onto the variables list:
  for(int i=0; i<nparams; i++)
    {
    chain_parameter_to_function(function_decl, types[i], names[i]);
    }

  // Establish the RESULT_DECL for the function:
  tree resdecl = build_decl (location_from_lineno(), RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resdecl) = function_decl;
  DECL_RESULT (function_decl) = resdecl;

  // The function_decl has a .function member, a pointer to struct_function.
  // This is quietly, almost invisibly, extremely important.  You need to
  // call this routine after DECL_RESULT has been established:

  allocate_struct_function(function_decl, false);

  struct gg_function_t new_function = {};
  new_function.context_count = 0;
  new_function.function_decl = function_decl;

  // Each program on the stack gets a unique identifier.  This is used, for
  // example, to make sure that static variables have unique names.
  static size_t program_id = 0;
  new_function.program_id_number = program_id++;

  // With everything established, put this function_decl on the stack
  gg_trans_unit.function_stack.push_back(new_function);

  // All we need is a context, and we are ready to go:
  gg_push_context();
  }

tree
gg_get_function_decl(tree return_type, const char *funcname, ...)
  {
  // This very similar routine creates and returns the function_decl

  // It was designed for implementing nested functions, in particular
  // in cases of forward references. Thus, you need to have the function_decl
  // in order to create the call_expr, even though you don't yet have a body,
  // and you aren't ready to create it at this time.

  int nparams = 0;

  tree types[ARG_LIMIT];
  const char *names[ARG_LIMIT];

  va_list params;
  va_start(params,funcname);
  for(;;)
    {
    tree var_type = va_arg(params, tree);
    if( !var_type )
      {
      break;
      }

    if( TREE_CODE(var_type) >= NUM_TREE_CODES)
      {
      // Warning:  This test is not completely reliable, because a garbage
      // byte could have a valid TREE_CODE.  But it does help.
      yywarn("You nitwit!");
      yywarn("You forgot to put a NULL_TREE at the end of a "
            "gg_define_function() again!");
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      gcc_unreachable();
      }

    const char *name = va_arg(params, const char *);

    types[nparams] = var_type;
    names[nparams] = name;
    nparams += 1;
    if(nparams > ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### %d parameters?  Really?  Are you insane?",
                  ARG_LIMIT+1);
      gcc_unreachable();
      }
    }
  va_end(params);

  // Create the FUNCTION_TYPE for that array:
  tree fndecl_type = build_varargs_function_type_array( return_type,
                     nparams,
                     types);

  // Create the FUNCTION_DECL for that FUNCTION_TYPE
  tree function_decl = build_fn_decl (funcname, fndecl_type);

  // Some of this stuff is magical, and is based on compiling C programs
  // and just mimicking the results.
  TREE_ADDRESSABLE(function_decl) = 1;
  TREE_STATIC(function_decl) = 1;
  DECL_EXTERNAL (function_decl) = 0;
  DECL_PRESERVE_P (function_decl) = 0;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(function_decl) = 1;
  DECL_ARTIFICIAL(function_decl) = 0;
  TREE_NOTHROW(function_decl) = 0;
  TREE_USED(function_decl) = 1;

  if( gg_trans_unit.function_stack.size() == 0 )
    {
    // gg_trans_unit.function_stack is empty, so our context is
    // the compilation module, and we need to be public:
    DECL_CONTEXT (function_decl) = gg_trans_unit.trans_unit_decl;
    TREE_PUBLIC(function_decl) = 1;
    }
  else
    {
    // The stack has something in it, so we are building a nested function.
    // Make the current function our context
    DECL_CONTEXT (function_decl) = current_function->function_decl;
    TREE_PUBLIC(function_decl) = 0;
    DECL_STATIC_CHAIN(function_decl) = 1;
    }

  // Chain the names onto the variables list:
  for(int i=0; i<nparams; i++)
    {
    chain_parameter_to_function(function_decl, types[i], names[i]);
    }

  // Establish the RESULT_DECL for the function:
  tree resdecl = build_decl (location_from_lineno(), RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resdecl) = function_decl;
  DECL_RESULT (function_decl) = resdecl;

  // The function_decl has a .function member, a pointer to struct_function.
  // This is quietly, almost invisibly, extremely important.  You need to
  // call this routine after DECL_RESULT has been established:
  allocate_struct_function(function_decl, false);

  // It will be the caller's responsibility to push this function_decl onto
  // the stack at the appropriate time, and create the appropriate context.
  return function_decl;
  }

void
gg_finalize_function()
  {
  // Unless it has already been handled:
  if( !gg_trans_unit.function_stack.size() )
    {
    return ;
    }

  // Finish off the context
  gg_pop_context();

  if( gg_trans_unit.function_stack.back().is_truly_nested )
    {
    // This code is for true nested functions.

    /////////  DANGER, WILL ROBINSON!
    /////////  This is all well and good.  It does not, however, work.
    /////////  I tried to implement it because I had a Brilliant Idea for
    /////////  building COBOL paragraphs in a way that would easily allow
    /////////  the GDB "NEXT" command to step over a PERFORM <paragraph>.
    /////////  But, alas, I realized that it was just not going to work.
    /////////
    /////////  Pity.
    /////////
    /////////  But at that point, I was here, and I am leaving this uncooked
    /////////  code in case I someday want to return to it.  If it becomes
    /////////  your job, rather than mine, I encourage you to write a C
    /////////  program that uses the GNU extensions that allow true nested
    /////////  functions, and reverse engineer the "finish_function"
    /////////  function, and get it working.
    /////////
    /////////  Good luck.  Bob Dubner, 2022-08-13

    // Because this is a nested function, let's make sure that it actually
    // has a function that it is nested within
    gcc_assert(gg_trans_unit.function_stack.size() > 1 );

    /* Genericize before inlining.  Delay genericizing nested functions
       until their parent function is genericized.  Since finalizing
       requires GENERIC, delay that as well.  */

    // This is the comment in gcc/c/c-decl.c:

    /* Register this function with cgraph just far enough to get it
    added to our parent's nested function list.  Handy, since the
    C front end doesn't have such a list.  */

    static cgraph_node *node = cgraph_node::get_create (current_function->function_decl);
    gcc_assert(node);

    }
  else
    {
    // This makes the function visible on the source code module level.
    cgraph_node::finalize_function (current_function->function_decl, true);
    }

  dump_function (TDI_original, current_function->function_decl);

  if( gg_trans_unit.function_stack.back().context_count )
    {
    cbl_internal_error("Residual context count!");
    }

  gg_trans_unit.function_stack.pop_back();
  }

void
gg_push_context()
  {
  // Sit back, relax, prepare to be amazed.

  // functions need a context in which they build variables and whatnot.
  // they also need to be able to create subcontexts.

  // Functions have an DECL_INITIAL member that points to the first block.  The
  // first block has a BLOCK_VARS member that points to the first of a chain
  // of var_decl entries.  The first block has a BLOCK_SUBBLOCKS member that
  // points to the block of the first subcontext.

  // Functions have a DECL_SAVED_TREE member that points to the first bind_expr
  // That first bind_expr has a BIND_EXPR_BLOCK that points back to the first block
  //                      has a BIND_EXPR_VARS that points back to the first block's first var_decl
  //                      has a BIND_EXPR_BODY that points to the first statement_list

  // Each subsequent context gets a new block that is chained to the prior block through BLOCK_SUBBLOCKS
  // Each subsequent context gets a new bind_expr which gets added to the parent context's statement list

  // Yes, it's confusing.  Have a nice lie-down.

  // Here's what we need for this recipe:

  // We need a block:
  tree block = make_node(BLOCK);
  TREE_USED(block) = 1;
  BLOCK_SUPERCONTEXT(block) = current_function->function_decl;

  // We need a statement list:
  tree statement_list = alloc_stmt_list();

  // We need a bind_expr:
  tree bind_expr = build3(BIND_EXPR,
                          void_type_node,
                          NULL_TREE,         // There are no vars yet.
                          statement_list,
                          block);
  TREE_SIDE_EFFECTS(bind_expr) = 1;

  // At this point, we might be creating the initial context for a function,
  // or we might be creating a sub-context.

  if( !DECL_INITIAL(current_function->function_decl) )
    {
    // We are creating the initial context of the function:
    DECL_INITIAL(current_function->function_decl) = block;
    DECL_SAVED_TREE(current_function->function_decl) = bind_expr;

    // To avoid an N-squared time complexity when chaining blocks, we save the
    // current end of the chain of blocks:
    current_function->current_block = block;
    }
  else
    {
    // We are in the subtext business:

    // We need to tack on our new block to the end of the
    // chain of existing blocks:
    tree cblock = current_function->current_block;
    BLOCK_SUBBLOCKS(cblock) = block;
    current_function->current_block = block;

    // And we need to put our new bind_expr onto the end of the
    // current active statement list:
    gg_append_statement(bind_expr);
    }

  // And now we make our statement_list and bind_expr the active ones:
  current_function->statement_list_stack.push_back(statement_list);
  current_function->bind_expr_stack.push_back(bind_expr);

  // And the new context is ready to rock and roll
  gg_trans_unit.function_stack.back().context_count += 1;
  }

void
gg_pop_context()
  {
  // Backing out is much easier:
  current_function->bind_expr_stack.pop_back();
  current_function->statement_list_stack.pop_back();

  gg_trans_unit.function_stack.back().context_count -= 1;
  }

static
std::unordered_map<std::string, tree> fndecl_from_name;

static
tree
function_decl_from_name(tree return_type,
                        const char *function_name,
                        int nargs,
                        tree arg_types[])
  {
  tree fndecl;
  std::unordered_map<std::string, tree>::const_iterator it =
                fndecl_from_name.find(function_name);
  if( it != fndecl_from_name.end() )
    {
    fndecl = it->second;
    }
  else
    {
    tree fntype = build_function_type_array(return_type, nargs, arg_types);
    fndecl = build_fn_decl (function_name, fntype);
    fndecl_from_name[function_name] = fndecl;
    }
  return fndecl;
  }

tree
gg_call_expr(tree return_type, const char *function_name, ...)
  {
  // Generalized caller. Params are terminated with NULL_TREE

  // Use this routine to call function_name when you need the return value.
  // Typically you will do something like

  //      tree call_expr = gg_call_expr(...);
  //      gg_assign( dest, call_expr );

  // Note that everyt time call_expr is laid down, the function will be called,
  // so you probably don't want to do things like
  //      gg_assign( dest1, call_expr );
  //      gg_assign( dest2, call_expr );

  int nargs = 0;
  static tree arg_types[ARG_LIMIT+1];
  static tree args[ARG_LIMIT+1];

  va_list ap;
  va_start(ap, function_name);
  for(;;)
    {
    if(nargs >= ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### You *must* be joking!");
      gcc_unreachable();
      }

    tree arg = va_arg(ap, tree);

    if( !arg )
      {
      break;
      }

    arg_types[nargs] = TREE_TYPE(arg);
    args[nargs]      = arg;
    nargs += 1;
    }
  arg_types[nargs] = NULL_TREE;
  args[nargs]      = NULL_TREE;
  va_end (ap);

  tree function_decl = function_decl_from_name( return_type,
                                              function_name,
                                              nargs,
                                              arg_types);
  DECL_EXTERNAL (function_decl) = 1;
  tree the_func_addr = build1(ADDR_EXPR,
                              build_pointer_type (TREE_TYPE(function_decl)),
                              function_decl);
  tree the_call = build_call_array_loc(location_from_lineno(),
                                       return_type,
                                       the_func_addr,
                                       nargs,
                                       args);
  // This routine returns the call_expr; the caller will have to deal with it
  // as described up above
  return the_call;
  }

void
gg_call(tree return_type, const char *function_name,  ...)
  {
  // Generalized caller. function_name is followed by a NULL_TREE-terminated
  // list of formal parameters.

  // Use this routine when you don't care about the return value, and
  // you want the subroutine to be invoked.

  int nargs = 0;
  static tree arg_types[ARG_LIMIT+1];
  static tree args[ARG_LIMIT+1];

  va_list ap;
  va_start(ap, function_name);
  for(;;)
    {
    if(nargs >= ARG_LIMIT)
      {
      yywarn("###### %10s in %s:%d", __func__, __FILE__,__LINE__ );
      yywarn("###### You *must* be joking!");
      gcc_unreachable();
      }

    tree arg = va_arg(ap, tree);

    if( !arg )
      {
      break;
      }

    arg_types[nargs] = TREE_TYPE(arg);
    args[nargs]      = arg;
    nargs += 1;
    }
  arg_types[nargs] = NULL_TREE;
  args[nargs]      = NULL_TREE;
  va_end (ap);

  tree function_decl = function_decl_from_name( return_type,
                                              function_name,
                                              nargs,
                                              arg_types);
  DECL_EXTERNAL (function_decl) = 1;
  tree the_func_addr = build1(ADDR_EXPR,
                              build_pointer_type (TREE_TYPE(function_decl)),
                              function_decl);
  tree the_call = build_call_array_loc(location_from_lineno(),
                                       return_type,
                                       the_func_addr,
                                       nargs,
                                       args);
  // This simply executes the_call; any return value is ignored
  gg_append_statement(the_call);
  }

tree
gg_call_expr_list(tree return_type, tree function_name, int param_count, tree args[])
  {
  // Generalized caller. param_count is the count of params in the arg[]]

  // Use this routine when you need the return value.  Typically you
  // will do something like

  //      tree call_expr_Plist = gg_call_expr_list(...);
  //      gg_append_statement(call_expr);

  // Note that every time call_expr is invoked, the routine will run again.

  // Avoid that with something like
  //      gg_assign( dest, gg_call_expr_list(...) );

  tree the_call = build_call_array_loc(location_from_lineno(),
                                       return_type,
                                       function_name,
                                       param_count,
                                       args);
  // This routine returns the call_expr; the caller will have to deal with it
  // as described up above
  return the_call;
  }

tree
gg_create_bind_expr()
  {
  // In support of things like PERFORM paragraph, we need to create
  // blocks of statements that can be executed.

  // This will be a naked bind_expr, like we use for WHILE construction.
  // It's not defining a context, so it has no variable list, nor does
  // it point to a block.

  tree statement_block = make_node(STATEMENT_LIST);
  TREE_TYPE(statement_block) = void_type_node;
  tree bind_expr = build3(   BIND_EXPR,
                             void_type_node,
                             NULL_TREE,
                             statement_block,
                             NULL_TREE);

  return bind_expr;
  }

void
gg_exit(tree exit_code)
  {
  tree the_call =
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_EXIT),
                          1,
                          exit_code);
  gg_append_statement(the_call);
  }

void
gg_abort()
  {
  tree the_call =
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_ABORT),
                          0);
  gg_append_statement(the_call);
  }

tree
gg_strlen(tree psz)
  {
  tree the_call = fold_convert(
      size_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_STRLEN),
                          1,
                          psz));
  return the_call;
  }

tree
gg_strdup(tree psz)
  {
  tree the_call = fold_convert(
      build_pointer_type(char_type_node),
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_STRDUP),
                          1,
                          psz));
  return the_call;
  }

/* built_in call to malloc() */

tree
gg_malloc(tree size)
  {
  tree the_call = fold_convert(
      pvoid_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_MALLOC),
                          1,
                          size));
  return the_call;
  }

tree
gg_realloc(tree base, tree size)
  {
  tree the_call = fold_convert(
      pvoid_type_node,
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_REALLOC),
                          2,
                          base,
                          size));
  return the_call;
  }

tree
gg_realloc(tree base, size_t size)
  {
  return gg_realloc(base, build_int_cst_type(SIZE_T, size));
  }

tree
gg_malloc(size_t size)
  {
  return gg_malloc(build_int_cst_type(SIZE_T, size));
  }

void
gg_free(tree pointer)
  {
  tree the_call =
      build_call_expr_loc(location_from_lineno(),
                          builtin_decl_explicit (BUILT_IN_FREE),
                          1,
                          pointer);
  gg_append_statement(the_call);
  }

void
gg_record_statement_list_start()
  {
  // We need a statement list:
  tree statement_list = alloc_stmt_list();
  current_function->statement_list_stack.push_back(statement_list);
  }

tree
gg_record_statement_list_finish()
  {
  tree retval = current_function->statement_list_stack.back();
  current_function->statement_list_stack.pop_back();
  return retval;
  }

size_t
gg_sizeof(tree node)
  {
  size_t size_in_bytes;
  if( DECL_P(node) )
    {
    size_in_bytes = TREE_INT_CST_LOW(TYPE_SIZE_UNIT(TREE_TYPE(node)));
    }
  else
    {
    gcc_assert(TYPE_P(node));
    size_in_bytes = TREE_INT_CST_LOW(TYPE_SIZE_UNIT(node));
    }
  return size_in_bytes;
  }

tree
gg_array_of_size_t( size_t N, size_t *values)
  {
  tree retval = gg_define_variable(build_pointer_type(SIZE_T));
  tree sz = build_int_cst_type(SIZE_T, N * int_size_in_bytes(SIZE_T));
  gg_assign(retval, gg_cast(build_pointer_type(SIZE_T), gg_malloc(sz)));
  for(size_t i=0; i<N; i++)
    {
    gg_assign(gg_array_value(retval, i), build_int_cst_type(SIZE_T, values[i]));
    }
  return retval;
  }

tree
gg_array_of_bytes( size_t N, unsigned char *values)
  {
  tree retval = gg_define_variable(UCHAR_P);
  gg_assign(retval, gg_cast(UCHAR_P, gg_malloc(  build_int_cst_type(SIZE_T, N))));
  for(size_t i=0; i<N; i++)
    {
    gg_assign(gg_array_value(retval, i), build_int_cst_type(UCHAR, values[i]));
    }
  return retval;
  }

tree
gg_string_literal(const char *string)
  {
  /*  This is a message in a bottle.

      A genapi.cc program calling

            gg_call(VOID,
              "puts",
              build_string_literal(strlen(ach)+1, ach),
              NULL_TREE);

      ten thousand times compiles about ten percent slower than a C program
      calling

            puts(ach);

      ten thousand times.

      Trapping through the C front end reveals that they do not call
      build_string_literal().  They instead use build_string() in a way that
      I gave up trying to figure out that produces, apparently, more efficient
      GENERIC.

      Their GENERIC: call_expr -> nop_expr -> addr_expr -> string_cst

      My GENERIC:    call_expr -> addr_expr -> array_ref -> string_cst

      I tried for an hour to duplicate the C stuff, but made no headway.

      This comment is a reminder to myself to investigate this, someday, because
      I eventually want that ten percent.
      */

  return build_string_literal(strlen(string)+1, string);
  }

void
gg_set_current_line_number(int line_number)
  {
  sv_current_line_number = line_number;
  }

int
gg_get_current_line_number()
  {
  return sv_current_line_number;
  }

tree
gg_trans_unit_var_decl(const char *var_name)
  {
  std::unordered_map<std::string, tree>::const_iterator it =
    gg_trans_unit.trans_unit_var_decls.find(var_name);
  if( it != gg_trans_unit.trans_unit_var_decls.end() )
    {
    return it->second;
    }
  return NULL_TREE;
  }

void
gg_insert_into_assembler(const char *format, ...)
  {
  // Temporarily defeat all ASM_EXPR for optimized code per PR119214
  // The correct solution using LABEL_DECL is forthcoming
  if( !optimize )
    {
    // This routine inserts text directly into the assembly language stream.

    // Note that if for some reason your text has to have a '%' character, it
    // needs to be doubled in the GENERIC tag.  And that means if it is in the
    // 'format' variable, it needs to be quadrupled.

    // Create the string to be inserted:
    char ach[256];
    va_list ap;
    va_start(ap, format);
    vsnprintf(ach, sizeof(ach), format, ap);
    va_end(ap);

    // Create the required generic tag
    tree asm_expr = build5_loc( location_from_lineno(),
                            ASM_EXPR,
                            VOID,
                            build_string(strlen(ach), ach),
                            NULL_TREE,
                            NULL_TREE,
                            NULL_TREE,
                            NULL_TREE);
    //SET_EXPR_LOCATION (asm_expr, UNKNOWN_LOCATION);

    // And insert it as a statement
    gg_append_statement(asm_expr);
    }
  }
