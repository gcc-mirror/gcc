/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                T R A N S                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "real.h"
#include "flags.h"
#include "toplev.h"
#include "rtl.h"
#include "expr.h"
#include "ggc.h"
#include "cgraph.h"
#include "function.h"
#include "except.h"
#include "debug.h"
#include "output.h"
#include "tree-gimple.h"
#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "snames.h"
#include "stringt.h"
#include "uintp.h"
#include "urealp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

int max_gnat_nodes;
int number_names;
struct Node *Nodes_Ptr;
Node_Id *Next_Node_Ptr;
Node_Id *Prev_Node_Ptr;
struct Elist_Header *Elists_Ptr;
struct Elmt_Item *Elmts_Ptr;
struct String_Entry *Strings_Ptr;
Char_Code *String_Chars_Ptr;
struct List_Header *List_Headers_Ptr;

/* Current filename without path. */
const char *ref_filename;

/* If true, then gigi is being called on an analyzed but unexpanded
   tree, and the only purpose of the call is to properly annotate
   types with representation information. */
bool type_annotate_only;

/* A structure used to gather together information about a statement group.
   We use this to gather related statements, for example the "then" part
   of a IF.  In the case where it represents a lexical scope, we may also
   have a BLOCK node corresponding to it and/or cleanups.  */

struct stmt_group GTY((chain_next ("%h.previous"))) {
  struct stmt_group *previous;	/* Previous code group.  */
  tree stmt_list;		/* List of statements for this code group. */
  tree block;			/* BLOCK for this code group, if any. */
  tree cleanups;		/* Cleanups for this code group, if any.  */
};

static GTY(()) struct stmt_group *current_stmt_group;

/* List of unused struct stmt_group nodes.  */
static GTY((deletable)) struct stmt_group *stmt_group_free_list;

/* A structure used to record information on elaboration procedures
   we've made and need to process.

   ??? gnat_node should be Node_Id, but gengtype gets confused.  */

struct elab_info GTY((chain_next ("%h.next"))) {
  struct elab_info *next;	/* Pointer to next in chain. */
  tree elab_proc;		/* Elaboration procedure.  */
  int gnat_node;		/* The N_Compilation_Unit.  */
};

static GTY(()) struct elab_info *elab_info_list;

/* Free list of TREE_LIST nodes used for stacks.  */
static GTY((deletable)) tree gnu_stack_free_list;

/* List of TREE_LIST nodes representing a stack of exception pointer
   variables.  TREE_VALUE is the VAR_DECL that stores the address of
   the raised exception.  Nonzero means we are in an exception
   handler.  Not used in the zero-cost case.  */
static GTY(()) tree gnu_except_ptr_stack;

/* List of TREE_LIST nodes used to store the current elaboration procedure
   decl.  TREE_VALUE is the decl.  */
static GTY(()) tree gnu_elab_proc_stack;

/* Variable that stores a list of labels to be used as a goto target instead of
   a return in some functions.  See processing for N_Subprogram_Body.  */
static GTY(()) tree gnu_return_label_stack;

/* List of TREE_LIST nodes representing a stack of LOOP_STMT nodes.
   TREE_VALUE of each entry is the label of the corresponding LOOP_STMT.  */
static GTY(()) tree gnu_loop_label_stack;

/* List of TREE_LIST nodes representing labels for switch statements.
   TREE_VALUE of each entry is the label at the end of the switch.  */
static GTY(()) tree gnu_switch_label_stack;

/* Map GNAT tree codes to GCC tree codes for simple expressions.  */
static enum tree_code gnu_codes[Number_Node_Kinds];

/* Current node being treated, in case abort called.  */
Node_Id error_gnat_node;

static void Compilation_Unit_to_gnu (Node_Id);
static void record_code_position (Node_Id);
static void insert_code_for (Node_Id);
static void start_stmt_group (void);
static void add_cleanup (tree);
static tree mark_visited (tree *, int *, void *);
static tree mark_unvisited (tree *, int *, void *);
static tree end_stmt_group (void);
static void add_stmt_list (List_Id);
static tree build_stmt_group (List_Id, bool);
static void push_stack (tree *, tree, tree);
static void pop_stack (tree *);
static enum gimplify_status gnat_gimplify_stmt (tree *);
static void elaborate_all_entities (Node_Id);
static void process_freeze_entity (Node_Id);
static void process_inlined_subprograms (Node_Id);
static void process_decls (List_Id, List_Id, Node_Id, bool, bool);
static tree emit_range_check (tree, Node_Id);
static tree emit_index_check (tree, tree, tree, tree);
static tree emit_check (tree, tree, int);
static tree convert_with_check (Entity_Id, tree, bool, bool, bool);
static bool addressable_p (tree);
static tree assoc_to_constructor (Node_Id, tree);
static tree extract_values (tree, tree);
static tree pos_to_constructor (Node_Id, tree, Entity_Id);
static tree maybe_implicit_deref (tree);
static tree gnat_stabilize_reference_1 (tree, bool);
static void annotate_with_node (tree, Node_Id);
static void build_global_cdtor (int, tree *);


/* This is the main program of the back-end.  It sets up all the table
   structures and then generates code.  */

void
gigi (Node_Id gnat_root, int max_gnat_node, int number_name,
      struct Node *nodes_ptr, Node_Id *next_node_ptr, Node_Id *prev_node_ptr,
      struct Elist_Header *elists_ptr, struct Elmt_Item *elmts_ptr,
      struct String_Entry *strings_ptr, Char_Code *string_chars_ptr,
      struct List_Header *list_headers_ptr, Int number_units ATTRIBUTE_UNUSED,
      char *file_info_ptr ATTRIBUTE_UNUSED, Entity_Id standard_integer,
      Entity_Id standard_long_long_float, Entity_Id standard_exception_type,
      Int gigi_operating_mode)
{
  tree gnu_standard_long_long_float;
  tree gnu_standard_exception_type;
  struct elab_info *info;

  max_gnat_nodes = max_gnat_node;
  number_names = number_name;
  Nodes_Ptr = nodes_ptr;
  Next_Node_Ptr = next_node_ptr;
  Prev_Node_Ptr = prev_node_ptr;
  Elists_Ptr = elists_ptr;
  Elmts_Ptr = elmts_ptr;
  Strings_Ptr = strings_ptr;
  String_Chars_Ptr = string_chars_ptr;
  List_Headers_Ptr = list_headers_ptr;

  type_annotate_only = (gigi_operating_mode == 1);

  init_gnat_to_gnu ();
  gnat_compute_largest_alignment ();
  init_dummy_type ();

  /* If we are just annotating types, give VOID_TYPE zero sizes to avoid
     errors.  */
  if (type_annotate_only)
    {
      TYPE_SIZE (void_type_node) = bitsize_zero_node;
      TYPE_SIZE_UNIT (void_type_node) = size_zero_node;
    }

  /* Save the type we made for integer as the type for Standard.Integer.
     Then make the rest of the standard types.  Note that some of these
     may be subtypes.  */
  save_gnu_tree (Base_Type (standard_integer), TYPE_NAME (integer_type_node),
		 false);

  gnu_except_ptr_stack = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE);

  gnu_standard_long_long_float
    = gnat_to_gnu_entity (Base_Type (standard_long_long_float), NULL_TREE, 0);
  gnu_standard_exception_type
    = gnat_to_gnu_entity (Base_Type (standard_exception_type),  NULL_TREE, 0);

  init_gigi_decls (gnu_standard_long_long_float, gnu_standard_exception_type);

  /* Process any Pragma Ident for the main unit.  */
#ifdef ASM_OUTPUT_IDENT
  if (Present (Ident_String (Main_Unit)))
    ASM_OUTPUT_IDENT
      (asm_out_file,
       TREE_STRING_POINTER (gnat_to_gnu (Ident_String (Main_Unit))));
#endif

  /* If we are using the GCC exception mechanism, let GCC know.  */
  if (Exception_Mechanism == Back_End_Exceptions)
    gnat_init_gcc_eh ();

  gcc_assert (Nkind (gnat_root) == N_Compilation_Unit);
  Compilation_Unit_to_gnu (gnat_root);

  /* Now see if we have any elaboration procedures to deal with. */
  for (info = elab_info_list; info; info = info->next)
    {
      tree gnu_body = DECL_SAVED_TREE (info->elab_proc);
      tree gnu_stmts;

      /* Mark everything we have as not visited.  */
      walk_tree_without_duplicates (&gnu_body, mark_unvisited, NULL);

      /* Set the current function to be the elaboration procedure and gimplify
	 what we have.  */
      current_function_decl = info->elab_proc;
      gimplify_body (&gnu_body, info->elab_proc, true);

      /* We should have a BIND_EXPR, but it may or may not have any statements
	 in it.  If it doesn't have any, we have nothing to do.  */
      gnu_stmts = gnu_body;
      if (TREE_CODE (gnu_stmts) == BIND_EXPR)
	gnu_stmts = BIND_EXPR_BODY (gnu_stmts);

      /* If there are no statements, there is no elaboration code.  */
      if (!gnu_stmts || !STATEMENT_LIST_HEAD (gnu_stmts))
	Set_Has_No_Elaboration_Code (info->gnat_node, 1);
      else
	{
	  /* Otherwise, compile the function.  Note that we'll be gimplifying
	     it twice, but that's fine for the nodes we use.  */
	  begin_subprog_body (info->elab_proc);
	  end_subprog_body (gnu_body);
	}
    }
}

/* Perform initializations for this module.  */

void
gnat_init_stmt_group ()
{
  /* Initialize ourselves.  */
  init_code_table ();
  start_stmt_group ();

  /* Enable GNAT stack checking method if needed */
  if (!Stack_Check_Probes_On_Target)
    set_stack_check_libfunc (gen_rtx_SYMBOL_REF (Pmode, "_gnat_stack_check"));
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Identifier,
   to a GCC tree, which is returned.  GNU_RESULT_TYPE_P is a pointer to
   where we should place the result type.  */

static tree
Identifier_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p)
{
  tree gnu_result_type;
  tree gnu_result;
  Node_Id gnat_temp, gnat_temp_type;

  /* If the Etype of this node does not equal the Etype of the Entity,
     something is wrong with the entity map, probably in generic
     instantiation. However, this does not apply to types. Since we sometime
     have strange Ekind's, just do this test for objects. Also, if the Etype of
     the Entity is private, the Etype of the N_Identifier is allowed to be the
     full type and also we consider a packed array type to be the same as the
     original type. Similarly, a class-wide type is equivalent to a subtype of
     itself. Finally, if the types are Itypes, one may be a copy of the other,
     which is also legal.  */
  gnat_temp = (Nkind (gnat_node) == N_Defining_Identifier
	       ? gnat_node : Entity (gnat_node));
  gnat_temp_type = Etype (gnat_temp);

  gcc_assert (Etype (gnat_node) == gnat_temp_type
	      || (Is_Packed (gnat_temp_type)
		  && Etype (gnat_node) == Packed_Array_Type (gnat_temp_type))
	      || (Is_Class_Wide_Type (Etype (gnat_node)))
	      || (IN (Ekind (gnat_temp_type), Private_Kind)
		  && Present (Full_View (gnat_temp_type))
		  && ((Etype (gnat_node) == Full_View (gnat_temp_type))
		      || (Is_Packed (Full_View (gnat_temp_type))
			  && (Etype (gnat_node)
			      == Packed_Array_Type (Full_View
						    (gnat_temp_type))))))
	      || (Is_Itype (Etype (gnat_node)) && Is_Itype (gnat_temp_type))
	      || !(Ekind (gnat_temp) == E_Variable
		   || Ekind (gnat_temp) == E_Component
		   || Ekind (gnat_temp) == E_Constant
		   || Ekind (gnat_temp) == E_Loop_Parameter
		   || IN (Ekind (gnat_temp), Formal_Kind)));

  /* If this is a reference to a deferred constant whose partial view is an
     unconstrained private type, the proper type is on the full view of the
     constant, not on the full view of the type, which may be unconstrained.

     This may be a reference to a type, for example in the prefix of the
     attribute Position, generated for dispatching code (see Make_DT in
     exp_disp,adb). In that case we need the type itself, not is parent,
     in particular if it is a derived type  */
  if (Is_Private_Type (gnat_temp_type)
      && Has_Unknown_Discriminants (gnat_temp_type)
      && Present (Full_View (gnat_temp))
      && !Is_Type (gnat_temp))
    {
      gnat_temp = Full_View (gnat_temp);
      gnat_temp_type = Etype (gnat_temp);
      gnu_result_type = get_unpadded_type (gnat_temp_type);
    }
  else
    {
      /* Expand the type of this identifier first, in case it is an enumeral
	 literal, which only get made when the type is expanded.  There is no
	 order-of-elaboration issue here.  We want to use the Actual_Subtype if
	 it has already been elaborated, otherwise the Etype.  Avoid using
	 Actual_Subtype for packed arrays to simplify things.  */
      if ((Ekind (gnat_temp) == E_Constant
	   || Ekind (gnat_temp) == E_Variable || Is_Formal (gnat_temp))
	  && !(Is_Array_Type (Etype (gnat_temp))
	       && Present (Packed_Array_Type (Etype (gnat_temp))))
	  && Present (Actual_Subtype (gnat_temp))
	  && present_gnu_tree (Actual_Subtype (gnat_temp)))
	gnat_temp_type = Actual_Subtype (gnat_temp);
      else
	gnat_temp_type = Etype (gnat_node);

      gnu_result_type = get_unpadded_type (gnat_temp_type);
    }

  gnu_result = gnat_to_gnu_entity (gnat_temp, NULL_TREE, 0);

  /* If we are in an exception handler, force this variable into memory to
     ensure optimization does not remove stores that appear redundant but are
     actually needed in case an exception occurs.

     ??? Note that we need not do this if the variable is declared within the
     handler, only if it is referenced in the handler and declared in an
     enclosing block, but we have no way of testing that right now.

     ??? Also, for now all we can do is make it volatile.  But we only
     do this for SJLJ.  */
  if (TREE_VALUE (gnu_except_ptr_stack)
      && TREE_CODE (gnu_result) == VAR_DECL)
    TREE_THIS_VOLATILE (gnu_result) = TREE_SIDE_EFFECTS (gnu_result) = 1;

  /* Some objects (such as parameters passed by reference, globals of
     variable size, and renamed objects) actually represent the address
     of the object.  In that case, we must do the dereference.  Likewise,
     deal with parameters to foreign convention subprograms.  Call fold
     here since GNU_RESULT may be a CONST_DECL.  */
  if (DECL_P (gnu_result)
      && (DECL_BY_REF_P (gnu_result)
	  || (TREE_CODE (gnu_result) == PARM_DECL
	      && DECL_BY_COMPONENT_PTR_P (gnu_result))))
    {
      bool ro = DECL_POINTS_TO_READONLY_P (gnu_result);
      tree renamed_obj;

      if (TREE_CODE (gnu_result) == PARM_DECL
	  && DECL_BY_COMPONENT_PTR_P (gnu_result))
	gnu_result
	  = build_unary_op (INDIRECT_REF, NULL_TREE,
			    convert (build_pointer_type (gnu_result_type),
				     gnu_result));

      /* If it's a renaming pointer and we are at the right binding level,
	 we can reference the renamed object directly, since the renamed
	 expression has been protected against multiple evaluations.  */
      else if (TREE_CODE (gnu_result) == VAR_DECL
	       && (renamed_obj = DECL_RENAMED_OBJECT (gnu_result)) != 0
	       && (! DECL_RENAMING_GLOBAL_P (gnu_result)
		   || global_bindings_p ()))
	gnu_result = renamed_obj;
      else
	gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE,
				     fold (gnu_result));

      TREE_READONLY (gnu_result) = TREE_STATIC (gnu_result) = ro;
    }

  /* The GNAT tree has the type of a function as the type of its result.  Also
     use the type of the result if the Etype is a subtype which is nominally
     unconstrained.  But remove any padding from the resulting type.  */
  if (TREE_CODE (TREE_TYPE (gnu_result)) == FUNCTION_TYPE
      || Is_Constr_Subt_For_UN_Aliased (gnat_temp_type))
    {
      gnu_result_type = TREE_TYPE (gnu_result);
      if (TREE_CODE (gnu_result_type) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (gnu_result_type))
	gnu_result_type = TREE_TYPE (TYPE_FIELDS (gnu_result_type));
    }

  /* We always want to return the underlying INTEGER_CST for an enumeration
     literal to avoid the need to call fold in lots of places.  But don't do
     this is the parent will be taking the address of this object.  */
  if (TREE_CODE (gnu_result) == CONST_DECL)
    {
      gnat_temp = Parent (gnat_node);
      if (!DECL_CONST_CORRESPONDING_VAR (gnu_result)
	  || (Nkind (gnat_temp) != N_Reference
	      && !(Nkind (gnat_temp) == N_Attribute_Reference
		   && ((Get_Attribute_Id (Attribute_Name (gnat_temp))
			== Attr_Address)
		       || (Get_Attribute_Id (Attribute_Name (gnat_temp))
			   == Attr_Access)
		       || (Get_Attribute_Id (Attribute_Name (gnat_temp))
			    == Attr_Unchecked_Access)
		       || (Get_Attribute_Id (Attribute_Name (gnat_temp))
			   == Attr_Unrestricted_Access)))))
	gnu_result = DECL_INITIAL (gnu_result);
    }

  *gnu_result_type_p = gnu_result_type;
  return gnu_result;
}

/* Subroutine of gnat_to_gnu to process gnat_node, an N_Pragma.  Return
   any statements we generate.  */

static tree
Pragma_to_gnu (Node_Id gnat_node)
{
  Node_Id gnat_temp;
  tree gnu_result = alloc_stmt_list ();

  /* Check for (and ignore) unrecognized pragma and do nothing if we are just
     annotating types.  */
  if (type_annotate_only || !Is_Pragma_Name (Chars (gnat_node)))
    return gnu_result;

  switch (Get_Pragma_Id (Chars (gnat_node)))
    {
    case Pragma_Inspection_Point:
      /* Do nothing at top level: all such variables are already viewable.  */
      if (global_bindings_p ())
	break;

      for (gnat_temp = First (Pragma_Argument_Associations (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	{
	  tree gnu_expr = gnat_to_gnu (Expression (gnat_temp));

	  if (TREE_CODE (gnu_expr) == UNCONSTRAINED_ARRAY_REF)
	    gnu_expr = TREE_OPERAND (gnu_expr, 0);

	  gnu_expr = build1 (USE_STMT, void_type_node, gnu_expr);
	  annotate_with_node (gnu_expr, gnat_node);
	  append_to_statement_list (gnu_expr, &gnu_result);
	}
      break;

    case Pragma_Optimize:
      switch (Chars (Expression
		     (First (Pragma_Argument_Associations (gnat_node)))))
	{
	case Name_Time:  case Name_Space:
	  if (optimize == 0)
	    post_error ("insufficient -O value?", gnat_node);
	  break;

	case Name_Off:
	  if (optimize != 0)
	    post_error ("must specify -O0?", gnat_node);
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    case Pragma_Reviewable:
      if (write_symbols == NO_DEBUG)
	post_error ("must specify -g?", gnat_node);
      break;
    }

  return gnu_result;
}
/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Attribute,
   to a GCC tree, which is returned.  GNU_RESULT_TYPE_P is a pointer to
   where we should place the result type.  ATTRIBUTE is the attribute ID.  */

static tree
Attribute_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p, int attribute)
{
  tree gnu_result = error_mark_node;
  tree gnu_result_type;
  tree gnu_expr;
  bool prefix_unused = false;
  tree gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
  tree gnu_type = TREE_TYPE (gnu_prefix);

  /* If the input is a NULL_EXPR, make a new one.  */
  if (TREE_CODE (gnu_prefix) == NULL_EXPR)
    {
      *gnu_result_type_p = get_unpadded_type (Etype (gnat_node));
      return build1 (NULL_EXPR, *gnu_result_type_p,
		     TREE_OPERAND (gnu_prefix, 0));
    }

  switch (attribute)
    {
    case Attr_Pos:
    case Attr_Val:
      /* These are just conversions until since representation clauses for
	 enumerations are handled in the front end.  */
      {
	bool checkp = Do_Range_Check (First (Expressions (gnat_node)));

	gnu_result = gnat_to_gnu (First (Expressions (gnat_node)));
	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_result = convert_with_check (Etype (gnat_node), gnu_result,
					 checkp, checkp, true);
      }
      break;

    case Attr_Pred:
    case Attr_Succ:
      /* These just add or subject the constant 1.  Representation clauses for
	 enumerations are handled in the front-end.  */
      gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      if (Do_Range_Check (First (Expressions (gnat_node))))
	{
	  gnu_expr = protect_multiple_eval (gnu_expr);
	  gnu_expr
	    = emit_check
	      (build_binary_op (EQ_EXPR, integer_type_node,
				gnu_expr,
				attribute == Attr_Pred
				? TYPE_MIN_VALUE (gnu_result_type)
				: TYPE_MAX_VALUE (gnu_result_type)),
	       gnu_expr, CE_Range_Check_Failed);
	}

      gnu_result
	= build_binary_op (attribute == Attr_Pred
			   ? MINUS_EXPR : PLUS_EXPR,
			   gnu_result_type, gnu_expr,
			   convert (gnu_result_type, integer_one_node));
      break;

    case Attr_Address:
    case Attr_Unrestricted_Access:
      /* Conversions don't change something's address but can cause us to miss
	 the COMPONENT_REF case below, so strip them off.  */
      gnu_prefix = remove_conversions (gnu_prefix,
				       !Must_Be_Byte_Aligned (gnat_node));

      /* If we are taking 'Address of an unconstrained object, this is the
	 pointer to the underlying array.  */
      if (attribute == Attr_Address)
	gnu_prefix = maybe_unconstrained_array (gnu_prefix);

      /* ... fall through ... */

    case Attr_Access:
    case Attr_Unchecked_Access:
    case Attr_Code_Address:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result
	= build_unary_op (((attribute == Attr_Address
			    || attribute == Attr_Unrestricted_Access)
			   && !Must_Be_Byte_Aligned (gnat_node))
			  ? ATTR_ADDR_EXPR : ADDR_EXPR,
			  gnu_result_type, gnu_prefix);

      /* For 'Code_Address, find an inner ADDR_EXPR and mark it so that we
	 don't try to build a trampoline.  */
      if (attribute == Attr_Code_Address)
	{
	  for (gnu_expr = gnu_result;
	       TREE_CODE (gnu_expr) == NOP_EXPR
	       || TREE_CODE (gnu_expr) == CONVERT_EXPR;
	       gnu_expr = TREE_OPERAND (gnu_expr, 0))
	    TREE_CONSTANT (gnu_expr) = 1;

	  if (TREE_CODE (gnu_expr) == ADDR_EXPR)
	    TREE_STATIC (gnu_expr) = TREE_CONSTANT (gnu_expr) = 1;
	}
      break;

    case Attr_Pool_Address:
      {
	tree gnu_obj_type;
	tree gnu_ptr = gnu_prefix;

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If this is an unconstrained array, we know the object must have been
	   allocated with the template in front of the object.  So compute the
	   template address.*/
	if (TYPE_FAT_POINTER_P (TREE_TYPE (gnu_ptr)))
	  gnu_ptr
	    = convert (build_pointer_type
		       (TYPE_OBJECT_RECORD_TYPE
			(TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (gnu_ptr)))),
		       gnu_ptr);

	gnu_obj_type = TREE_TYPE (TREE_TYPE (gnu_ptr));
	if (TREE_CODE (gnu_obj_type) == RECORD_TYPE
	    && TYPE_CONTAINS_TEMPLATE_P (gnu_obj_type))
	  {
	    tree gnu_char_ptr_type = build_pointer_type (char_type_node);
	    tree gnu_pos = byte_position (TYPE_FIELDS (gnu_obj_type));
	    tree gnu_byte_offset
	      = convert (gnu_char_ptr_type,
			 size_diffop (size_zero_node, gnu_pos));

	    gnu_ptr = convert (gnu_char_ptr_type, gnu_ptr);
	    gnu_ptr = build_binary_op (MINUS_EXPR, gnu_char_ptr_type,
				       gnu_ptr, gnu_byte_offset);
	  }

	gnu_result = convert (gnu_result_type, gnu_ptr);
      }
      break;

    case Attr_Size:
    case Attr_Object_Size:
    case Attr_Value_Size:
    case Attr_Max_Size_In_Storage_Elements:
      gnu_expr = gnu_prefix;

      /* Remove NOPS from gnu_expr and conversions from gnu_prefix.
	 We only use GNU_EXPR to see if a COMPONENT_REF was involved. */
      while (TREE_CODE (gnu_expr) == NOP_EXPR)
	gnu_expr = TREE_OPERAND (gnu_expr, 0)
	  ;

      gnu_prefix = remove_conversions (gnu_prefix, true);
      prefix_unused = true;
      gnu_type = TREE_TYPE (gnu_prefix);

      /* Replace an unconstrained array type with the type of the underlying
	 array.  We can't do this with a call to maybe_unconstrained_array
	 since we may have a TYPE_DECL.  For 'Max_Size_In_Storage_Elements,
	 use the record type that will be used to allocate the object and its
	 template.  */
      if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	{
	  gnu_type = TYPE_OBJECT_RECORD_TYPE (gnu_type);
	  if (attribute != Attr_Max_Size_In_Storage_Elements)
	    gnu_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_type)));
	}

      /* If we're looking for the size of a field, return the field size.
	 Otherwise, if the prefix is an object, or if 'Object_Size or
	 'Max_Size_In_Storage_Elements has been specified, the result is the
	 GCC size of the type. Otherwise, the result is the RM_Size of the
	 type.  */
      if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	gnu_result = DECL_SIZE (TREE_OPERAND (gnu_prefix, 1));
      else if (TREE_CODE (gnu_prefix) != TYPE_DECL
	       || attribute == Attr_Object_Size
	       || attribute == Attr_Max_Size_In_Storage_Elements)
	{
	  /* If this is a padded type, the GCC size isn't relevant to the
	     programmer.  Normally, what we want is the RM_Size, which was set
	     from the specified size, but if it was not set, we want the size
	     of the relevant field.  Using the MAX of those two produces the
	     right result in all case.  Don't use the size of the field if it's
	     a self-referential type, since that's never what's wanted.  */
	  if (TREE_CODE (gnu_type) == RECORD_TYPE
	      && TYPE_IS_PADDING_P (gnu_type)
	      && TREE_CODE (gnu_expr) == COMPONENT_REF)
	    {
	      gnu_result = rm_size (gnu_type);
	      if (!(CONTAINS_PLACEHOLDER_P
		    (DECL_SIZE (TREE_OPERAND (gnu_expr, 1)))))
		gnu_result
		  = size_binop (MAX_EXPR, gnu_result,
				DECL_SIZE (TREE_OPERAND (gnu_expr, 1)));
	    }
	  else if (Nkind (Prefix (gnat_node)) == N_Explicit_Dereference)
	    {
	      Node_Id gnat_deref = Prefix (gnat_node);
	      Node_Id gnat_actual_subtype = Actual_Designated_Subtype (gnat_deref);
	      tree gnu_ptr_type = TREE_TYPE (gnat_to_gnu (Prefix (gnat_deref)));
	      if (TYPE_FAT_OR_THIN_POINTER_P (gnu_ptr_type)
	  	&& Present (gnat_actual_subtype))
	        {
		  tree gnu_actual_obj_type = gnat_to_gnu_type (gnat_actual_subtype);
		  gnu_type = build_unc_object_type_from_ptr (gnu_ptr_type,
			       gnu_actual_obj_type, get_identifier ("SIZE"));
	        }

	      gnu_result = TYPE_SIZE (gnu_type);
	    }
	  else
	    gnu_result = TYPE_SIZE (gnu_type);
	}
      else
	gnu_result = rm_size (gnu_type);

      gcc_assert (gnu_result);

      /* Deal with a self-referential size by returning the maximum size for a
	 type and by qualifying the size with the object for 'Size of an
	 object.  */
      if (CONTAINS_PLACEHOLDER_P (gnu_result))
	{
	  if (TREE_CODE (gnu_prefix) != TYPE_DECL)
	    gnu_result = substitute_placeholder_in_expr (gnu_result, gnu_expr);
	  else
	    gnu_result = max_size (gnu_result, true);
	}

      /* If the type contains a template, subtract its size.  */
      if (TREE_CODE (gnu_type) == RECORD_TYPE
	  && TYPE_CONTAINS_TEMPLATE_P (gnu_type))
	gnu_result = size_binop (MINUS_EXPR, gnu_result,
				 DECL_SIZE (TYPE_FIELDS (gnu_type)));

      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* Always perform division using unsigned arithmetic as the size cannot
	 be negative, but may be an overflowed positive value. This provides
	 correct results for sizes up to 512 MB.

	 ??? Size should be calculated in storage elements directly.  */

      if (attribute == Attr_Max_Size_In_Storage_Elements)
	gnu_result = convert (sizetype,
			      fold (build2 (CEIL_DIV_EXPR, bitsizetype,
					    gnu_result, bitsize_unit_node)));
      break;

    case Attr_Alignment:
      if (TREE_CODE (gnu_prefix) == COMPONENT_REF
	  && (TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))
	      == RECORD_TYPE)
	  && (TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

      gnu_type = TREE_TYPE (gnu_prefix);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      prefix_unused = true;

      if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	gnu_result = size_int (DECL_ALIGN (TREE_OPERAND (gnu_prefix, 1)));
      else
	gnu_result = size_int (TYPE_ALIGN (gnu_type) / BITS_PER_UNIT);
      break;

    case Attr_First:
    case Attr_Last:
    case Attr_Range_Length:
      prefix_unused = true;

      if (INTEGRAL_TYPE_P (gnu_type) || TREE_CODE (gnu_type) == REAL_TYPE)
	{
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));

	  if (attribute == Attr_First)
	    gnu_result = TYPE_MIN_VALUE (gnu_type);
	  else if (attribute == Attr_Last)
	    gnu_result = TYPE_MAX_VALUE (gnu_type);
	  else
	    gnu_result
	      = build_binary_op
		(MAX_EXPR, get_base_type (gnu_result_type),
		 build_binary_op
		 (PLUS_EXPR, get_base_type (gnu_result_type),
		  build_binary_op (MINUS_EXPR,
				   get_base_type (gnu_result_type),
				   convert (gnu_result_type,
					    TYPE_MAX_VALUE (gnu_type)),
				   convert (gnu_result_type,
					    TYPE_MIN_VALUE (gnu_type))),
		  convert (gnu_result_type, integer_one_node)),
		 convert (gnu_result_type, integer_zero_node));

	  break;
	}

      /* ... fall through ... */

    case Attr_Length:
      {
	int Dimension = (Present (Expressions (gnat_node))
			 ? UI_To_Int (Intval (First (Expressions (gnat_node))))
			 : 1);

	/* Make sure any implicit dereference gets done.  */
	gnu_prefix = maybe_implicit_deref (gnu_prefix);
	gnu_prefix = maybe_unconstrained_array (gnu_prefix);
	gnu_type = TREE_TYPE (gnu_prefix);
	prefix_unused = true;
	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (TYPE_CONVENTION_FORTRAN_P (gnu_type))
	  {
	    int ndim;
	    tree gnu_type_temp;

	    for (ndim = 1, gnu_type_temp = gnu_type;
		 TREE_CODE (TREE_TYPE (gnu_type_temp)) == ARRAY_TYPE
		 && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type_temp));
		 ndim++, gnu_type_temp = TREE_TYPE (gnu_type_temp))
	      ;

	    Dimension = ndim + 1 - Dimension;
	  }

	for (; Dimension > 1; Dimension--)
	  gnu_type = TREE_TYPE (gnu_type);

	gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);
	if (attribute == Attr_First)
	  gnu_result
	    = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	else if (attribute == Attr_Last)
	  gnu_result
	    = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	else
	  /* 'Length or 'Range_Length.  */
	  {
	    tree gnu_compute_type
	      = gnat_signed_or_unsigned_type (0,
					      get_base_type (gnu_result_type));

	    gnu_result
	      = build_binary_op
		(MAX_EXPR, gnu_compute_type,
		 build_binary_op
		 (PLUS_EXPR, gnu_compute_type,
		  build_binary_op
		  (MINUS_EXPR, gnu_compute_type,
		   convert (gnu_compute_type,
			    TYPE_MAX_VALUE
			    (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)))),
		   convert (gnu_compute_type,
			    TYPE_MIN_VALUE
			    (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))))),
		  convert (gnu_compute_type, integer_one_node)),
		 convert (gnu_compute_type, integer_zero_node));
	  }

	/* If this has a PLACEHOLDER_EXPR, qualify it by the object we are
	   handling.  Note that these attributes could not have been used on
	   an unconstrained array type.  */
	gnu_result = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_result,
						     gnu_prefix);
	break;
      }

    case Attr_Bit_Position:
    case Attr_Position:
    case Attr_First_Bit:
    case Attr_Last_Bit:
    case Attr_Bit:
      {
	HOST_WIDE_INT bitsize;
	HOST_WIDE_INT bitpos;
	tree gnu_offset;
	tree gnu_field_bitpos;
	tree gnu_field_offset;
	tree gnu_inner;
	enum machine_mode mode;
	int unsignedp, volatilep;

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_prefix = remove_conversions (gnu_prefix, true);
	prefix_unused = true;

	/* We can have 'Bit on any object, but if it isn't a COMPONENT_REF,
	   the result is 0.  Don't allow 'Bit on a bare component, though. */
	if (attribute == Attr_Bit
	    && TREE_CODE (gnu_prefix) != COMPONENT_REF
	    && TREE_CODE (gnu_prefix) != FIELD_DECL)
	  {
	    gnu_result = integer_zero_node;
	    break;
	  }

	else
	  gcc_assert (TREE_CODE (gnu_prefix) == COMPONENT_REF
		      || (attribute == Attr_Bit_Position
			  && TREE_CODE (gnu_prefix) == FIELD_DECL));

	get_inner_reference (gnu_prefix, &bitsize, &bitpos, &gnu_offset,
			     &mode, &unsignedp, &volatilep, false);

	if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	  {
	    gnu_field_bitpos = bit_position (TREE_OPERAND (gnu_prefix, 1));
	    gnu_field_offset = byte_position (TREE_OPERAND (gnu_prefix, 1));

	    for (gnu_inner = TREE_OPERAND (gnu_prefix, 0);
		 TREE_CODE (gnu_inner) == COMPONENT_REF
		 && DECL_INTERNAL_P (TREE_OPERAND (gnu_inner, 1));
		 gnu_inner = TREE_OPERAND (gnu_inner, 0))
	      {
		gnu_field_bitpos
		  = size_binop (PLUS_EXPR, gnu_field_bitpos,
				bit_position (TREE_OPERAND (gnu_inner, 1)));
		gnu_field_offset
		  = size_binop (PLUS_EXPR, gnu_field_offset,
				byte_position (TREE_OPERAND (gnu_inner, 1)));
	      }
	  }
	else if (TREE_CODE (gnu_prefix) == FIELD_DECL)
	  {
	    gnu_field_bitpos = bit_position (gnu_prefix);
	    gnu_field_offset = byte_position (gnu_prefix);
	  }
	else
	  {
	    gnu_field_bitpos = bitsize_zero_node;
	    gnu_field_offset = size_zero_node;
	  }

	switch (attribute)
	  {
	  case Attr_Position:
	    gnu_result = gnu_field_offset;
	    break;

	  case Attr_First_Bit:
	  case Attr_Bit:
	    gnu_result = size_int (bitpos % BITS_PER_UNIT);
	    break;

	  case Attr_Last_Bit:
	    gnu_result = bitsize_int (bitpos % BITS_PER_UNIT);
	    gnu_result = size_binop (PLUS_EXPR, gnu_result,
				     TYPE_SIZE (TREE_TYPE (gnu_prefix)));
	    gnu_result = size_binop (MINUS_EXPR, gnu_result,
				     bitsize_one_node);
	    break;

	  case Attr_Bit_Position:
	    gnu_result = gnu_field_bitpos;
	    break;
		}

	/* If this has a PLACEHOLDER_EXPR, qualify it by the object
	   we are handling. */
	gnu_result = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_result, gnu_prefix);
	break;
      }

    case Attr_Min:
    case Attr_Max:
      {
	tree gnu_lhs = gnat_to_gnu (First (Expressions (gnat_node)));
	tree gnu_rhs = gnat_to_gnu (Next (First (Expressions (gnat_node))));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_result = build_binary_op (attribute == Attr_Min
				      ? MIN_EXPR : MAX_EXPR,
				      gnu_result_type, gnu_lhs, gnu_rhs);
      }
      break;

    case Attr_Passed_By_Reference:
      gnu_result = size_int (default_pass_by_ref (gnu_type)
			     || must_pass_by_ref (gnu_type));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case Attr_Component_Size:
      if (TREE_CODE (gnu_prefix) == COMPONENT_REF
	  && (TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))
	      == RECORD_TYPE)
	  && (TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

      gnu_prefix = maybe_implicit_deref (gnu_prefix);
      gnu_type = TREE_TYPE (gnu_prefix);

      if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	gnu_type = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_type))));

      while (TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type)))
	gnu_type = TREE_TYPE (gnu_type);

      gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);

      /* Note this size cannot be self-referential.  */
      gnu_result = TYPE_SIZE (TREE_TYPE (gnu_type));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      prefix_unused = true;
      break;

    case Attr_Null_Parameter:
      /* This is just a zero cast to the pointer type for
	 our prefix and dereferenced.  */
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result
	= build_unary_op (INDIRECT_REF, NULL_TREE,
			  convert (build_pointer_type (gnu_result_type),
				   integer_zero_node));
      TREE_PRIVATE (gnu_result) = 1;
      break;

    case Attr_Mechanism_Code:
      {
	int code;
	Entity_Id gnat_obj = Entity (Prefix (gnat_node));

	prefix_unused = true;
	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	if (Present (Expressions (gnat_node)))
	  {
	    int i = UI_To_Int (Intval (First (Expressions (gnat_node))));

	    for (gnat_obj = First_Formal (gnat_obj); i > 1;
		 i--, gnat_obj = Next_Formal (gnat_obj))
	      ;
	  }

	code = Mechanism (gnat_obj);
	if (code == Default)
	  code = ((present_gnu_tree (gnat_obj)
		   && (DECL_BY_REF_P (get_gnu_tree (gnat_obj))
		       || ((TREE_CODE (get_gnu_tree (gnat_obj))
			    == PARM_DECL)
			   && (DECL_BY_COMPONENT_PTR_P
			       (get_gnu_tree (gnat_obj))))))
		  ? By_Reference : By_Copy);
	gnu_result = convert (gnu_result_type, size_int (- code));
      }
      break;

    default:
      /* Say we have an unimplemented attribute.  Then set the value to be
	 returned to be a zero and hope that's something we can convert to the
	 type of this attribute.  */
      post_error ("unimplemented attribute", gnat_node);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = integer_zero_node;
      break;
    }

  /* If this is an attribute where the prefix was unused, force a use of it if
     it has a side-effect.  But don't do it if the prefix is just an entity
     name.  However, if an access check is needed, we must do it.  See second
     example in AARM 11.6(5.e). */
  if (prefix_unused && TREE_SIDE_EFFECTS (gnu_prefix)
      && !Is_Entity_Name (Prefix (gnat_node)))
    gnu_result = fold (build2 (COMPOUND_EXPR, TREE_TYPE (gnu_result),
			       gnu_prefix, gnu_result));

  *gnu_result_type_p = gnu_result_type;
  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Case_Statement,
   to a GCC tree, which is returned.  */

static tree
Case_Statement_to_gnu (Node_Id gnat_node)
{
  tree gnu_result;
  tree gnu_expr;
  Node_Id gnat_when;

  gnu_expr = gnat_to_gnu (Expression (gnat_node));
  gnu_expr = convert (get_base_type (TREE_TYPE (gnu_expr)), gnu_expr);

  /*  The range of values in a case statement is determined by the rules in
      RM 5.4(7-9). In almost all cases, this range is represented by the Etype
      of the expression. One exception arises in the case of a simple name that
      is parenthesized. This still has the Etype of the name, but since it is
      not a name, para 7 does not apply, and we need to go to the base type.
      This is the only case where parenthesization affects the dynamic
      semantics (i.e. the range of possible values at runtime that is covered
      by the others alternative.

      Another exception is if the subtype of the expression is non-static.  In
      that case, we also have to use the base type.  */
  if (Paren_Count (Expression (gnat_node)) != 0
      || !Is_OK_Static_Subtype (Underlying_Type
				(Etype (Expression (gnat_node)))))
    gnu_expr = convert (get_base_type (TREE_TYPE (gnu_expr)), gnu_expr);

  /* We build a SWITCH_EXPR that contains the code with interspersed
     CASE_LABEL_EXPRs for each label.  */

  push_stack (&gnu_switch_label_stack, NULL_TREE, create_artificial_label ());
  start_stmt_group ();
  for (gnat_when = First_Non_Pragma (Alternatives (gnat_node));
       Present (gnat_when);
       gnat_when = Next_Non_Pragma (gnat_when))
    {
      Node_Id gnat_choice;

      /* First compile all the different case choices for the current WHEN
	 alternative.  */
      for (gnat_choice = First (Discrete_Choices (gnat_when));
	   Present (gnat_choice); gnat_choice = Next (gnat_choice))
	{
	  tree gnu_low = NULL_TREE, gnu_high = NULL_TREE;

	  switch (Nkind (gnat_choice))
	    {
	    case N_Range:
	      gnu_low = gnat_to_gnu (Low_Bound (gnat_choice));
	      gnu_high = gnat_to_gnu (High_Bound (gnat_choice));
	      break;

	    case N_Subtype_Indication:
	      gnu_low = gnat_to_gnu (Low_Bound (Range_Expression
						(Constraint (gnat_choice))));
	      gnu_high = gnat_to_gnu (High_Bound (Range_Expression
						  (Constraint (gnat_choice))));
	      break;

	    case N_Identifier:
	    case N_Expanded_Name:
	      /* This represents either a subtype range or a static value of
		 some kind; Ekind says which.  If a static value, fall through
		 to the next case.  */
	      if (IN (Ekind (Entity (gnat_choice)), Type_Kind))
		{
		  tree gnu_type = get_unpadded_type (Entity (gnat_choice));

		  gnu_low = fold (TYPE_MIN_VALUE (gnu_type));
		  gnu_high = fold (TYPE_MAX_VALUE (gnu_type));
		  break;
		}

	      /* ... fall through ... */

	    case N_Character_Literal:
	    case N_Integer_Literal:
	      gnu_low = gnat_to_gnu (gnat_choice);
	      break;

	    case N_Others_Choice:
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  add_stmt_with_node (build3 (CASE_LABEL_EXPR, void_type_node,
				      gnu_low, gnu_high,
				      create_artificial_label ()),
			      gnat_choice);
	}

      /* Push a binding level here in case variables are declared since we want
	 them to be local to this set of statements instead of the block
	 containing the Case statement.  */
      add_stmt (build_stmt_group (Statements (gnat_when), true));
      add_stmt (build1 (GOTO_EXPR, void_type_node,
			TREE_VALUE (gnu_switch_label_stack)));
    }

  /* Now emit a definition of the label all the cases branched to. */
  add_stmt (build1 (LABEL_EXPR, void_type_node,
		    TREE_VALUE (gnu_switch_label_stack)));
  gnu_result = build3 (SWITCH_EXPR, TREE_TYPE (gnu_expr), gnu_expr,
		       end_stmt_group (), NULL_TREE);
  pop_stack (&gnu_switch_label_stack);

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Loop_Statement,
   to a GCC tree, which is returned.  */

static tree
Loop_Statement_to_gnu (Node_Id gnat_node)
{
  /* ??? It would be nice to use "build" here, but there's no build5.  */
  tree gnu_loop_stmt = build_nt (LOOP_STMT, NULL_TREE, NULL_TREE,
				 NULL_TREE, NULL_TREE, NULL_TREE);
  tree gnu_loop_var = NULL_TREE;
  Node_Id gnat_iter_scheme = Iteration_Scheme (gnat_node);
  tree gnu_cond_expr = NULL_TREE;
  tree gnu_result;

  TREE_TYPE (gnu_loop_stmt) = void_type_node;
  TREE_SIDE_EFFECTS (gnu_loop_stmt) = 1;
  LOOP_STMT_LABEL (gnu_loop_stmt) = create_artificial_label ();
  annotate_with_node (gnu_loop_stmt, gnat_node);

  /* Save the end label of this LOOP_STMT in a stack so that the corresponding
     N_Exit_Statement can find it.  */
  push_stack (&gnu_loop_label_stack, NULL_TREE,
	      LOOP_STMT_LABEL (gnu_loop_stmt));

  /* Set the condition that under which the loop should continue.
     For "LOOP .... END LOOP;" the condition is always true.  */
  if (No (gnat_iter_scheme))
    ;
  /* The case "WHILE condition LOOP ..... END LOOP;" */
  else if (Present (Condition (gnat_iter_scheme)))
    LOOP_STMT_TOP_COND (gnu_loop_stmt)
      = gnat_to_gnu (Condition (gnat_iter_scheme));
  else
    {
      /* We have an iteration scheme.  */
      Node_Id gnat_loop_spec = Loop_Parameter_Specification (gnat_iter_scheme);
      Entity_Id gnat_loop_var = Defining_Entity (gnat_loop_spec);
      Entity_Id gnat_type = Etype (gnat_loop_var);
      tree gnu_type = get_unpadded_type (gnat_type);
      tree gnu_low = TYPE_MIN_VALUE (gnu_type);
      tree gnu_high = TYPE_MAX_VALUE (gnu_type);
      bool reversep = Reverse_Present (gnat_loop_spec);
      tree gnu_first = reversep ? gnu_high : gnu_low;
      tree gnu_last = reversep ? gnu_low : gnu_high;
      enum tree_code end_code = reversep ? GE_EXPR : LE_EXPR;
      tree gnu_base_type = get_base_type (gnu_type);
      tree gnu_limit = (reversep ? TYPE_MIN_VALUE (gnu_base_type)
			: TYPE_MAX_VALUE (gnu_base_type));

      /* We know the loop variable will not overflow if GNU_LAST is a constant
	 and is not equal to GNU_LIMIT.  If it might overflow, we have to move
	 the limit test to the end of the loop.  In that case, we have to test
	 for an empty loop outside the loop.  */
      if (TREE_CODE (gnu_last) != INTEGER_CST
	  || TREE_CODE (gnu_limit) != INTEGER_CST
	  || tree_int_cst_equal (gnu_last, gnu_limit))
	{
	  gnu_cond_expr
	    = build3 (COND_EXPR, void_type_node,
		      build_binary_op (LE_EXPR, integer_type_node,
				       gnu_low, gnu_high),
		      NULL_TREE, alloc_stmt_list ());
	  annotate_with_node (gnu_cond_expr, gnat_loop_spec);
	}

      /* Open a new nesting level that will surround the loop to declare the
	 loop index variable.  */
      start_stmt_group ();
      gnat_pushlevel ();

      /* Declare the loop index and set it to its initial value.  */
      gnu_loop_var = gnat_to_gnu_entity (gnat_loop_var, gnu_first, 1);
      if (DECL_BY_REF_P (gnu_loop_var))
	gnu_loop_var = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_loop_var);

      /* The loop variable might be a padded type, so use `convert' to get a
	 reference to the inner variable if so.  */
      gnu_loop_var = convert (get_base_type (gnu_type), gnu_loop_var);

      /* Set either the top or bottom exit condition as appropriate depending
	 on whether or not we know an overflow cannot occur. */
      if (gnu_cond_expr)
	LOOP_STMT_BOT_COND (gnu_loop_stmt)
	  = build_binary_op (NE_EXPR, integer_type_node,
			     gnu_loop_var, gnu_last);
      else
	LOOP_STMT_TOP_COND (gnu_loop_stmt)
	  = build_binary_op (end_code, integer_type_node,
			     gnu_loop_var, gnu_last);

      LOOP_STMT_UPDATE (gnu_loop_stmt)
	= build_binary_op (reversep ? PREDECREMENT_EXPR
			   : PREINCREMENT_EXPR,
			   TREE_TYPE (gnu_loop_var),
			   gnu_loop_var,
			   convert (TREE_TYPE (gnu_loop_var),
				    integer_one_node));
      annotate_with_node (LOOP_STMT_UPDATE (gnu_loop_stmt),
			  gnat_iter_scheme);
    }

  /* If the loop was named, have the name point to this loop.  In this case,
     the association is not a ..._DECL node, but the end label from this
     LOOP_STMT. */
  if (Present (Identifier (gnat_node)))
    save_gnu_tree (Entity (Identifier (gnat_node)),
		   LOOP_STMT_LABEL (gnu_loop_stmt), true);

  /* Make the loop body into its own block, so any allocated storage will be
     released every iteration.  This is needed for stack allocation.  */
  LOOP_STMT_BODY (gnu_loop_stmt)
    = build_stmt_group (Statements (gnat_node), true);

  /* If we declared a variable, then we are in a statement group for that
     declaration.  Add the LOOP_STMT to it and make that the "loop".  */
  if (gnu_loop_var)
    {
      add_stmt (gnu_loop_stmt);
      gnat_poplevel ();
      gnu_loop_stmt = end_stmt_group ();
    }

  /* If we have an outer COND_EXPR, that's our result and this loop is its
     "true" statement.  Otherwise, the result is the LOOP_STMT. */
  if (gnu_cond_expr)
    {
      COND_EXPR_THEN (gnu_cond_expr) = gnu_loop_stmt;
      gnu_result = gnu_cond_expr;
      recalculate_side_effects (gnu_cond_expr);
    }
  else
    gnu_result = gnu_loop_stmt;

  pop_stack (&gnu_loop_label_stack);

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to process gnat_node, an N_Subprogram_Body.  We
   don't return anything.  */

static void
Subprogram_Body_to_gnu (Node_Id gnat_node)
{
  /* Save debug output mode in case it is reset.  */
  enum debug_info_type save_write_symbols = write_symbols;
  const struct gcc_debug_hooks *const save_debug_hooks = debug_hooks;
  /* Defining identifier of a parameter to the subprogram.  */
  Entity_Id gnat_param;
  /* The defining identifier for the subprogram body. Note that if a
     specification has appeared before for this body, then the identifier
     occurring in that specification will also be a defining identifier and all
     the calls to this subprogram will point to that specification.  */
  Entity_Id gnat_subprog_id
    = (Present (Corresponding_Spec (gnat_node))
       ? Corresponding_Spec (gnat_node) : Defining_Entity (gnat_node));
  /* The FUNCTION_DECL node corresponding to the subprogram spec.   */
  tree gnu_subprog_decl;
  /* The FUNCTION_TYPE node corresponding to the subprogram spec.  */
  tree gnu_subprog_type;
  tree gnu_cico_list;
  tree gnu_result;

  /* If this is a generic object or if it has been eliminated,
     ignore it.  */
  if (Ekind (gnat_subprog_id) == E_Generic_Procedure
      || Ekind (gnat_subprog_id) == E_Generic_Function
      || Is_Eliminated (gnat_subprog_id))
    return;

  /* If debug information is suppressed for the subprogram, turn debug
     mode off for the duration of processing.  */
  if (!Needs_Debug_Info (gnat_subprog_id))
    {
      write_symbols = NO_DEBUG;
      debug_hooks = &do_nothing_debug_hooks;
    }

  /* If this subprogram acts as its own spec, define it.  Otherwise, just get
     the already-elaborated tree node.  However, if this subprogram had its
     elaboration deferred, we will already have made a tree node for it.  So
     treat it as not being defined in that case.  Such a subprogram cannot
     have an address clause or a freeze node, so this test is safe, though it
     does disable some otherwise-useful error checking.  */
  gnu_subprog_decl
    = gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE,
			  Acts_As_Spec (gnat_node)
			  && !present_gnu_tree (gnat_subprog_id));

  gnu_subprog_type = TREE_TYPE (gnu_subprog_decl);

  /* Set the line number in the decl to correspond to that of the body so that
     the line number notes are written
     correctly.  */
  Sloc_to_locus (Sloc (gnat_node), &DECL_SOURCE_LOCATION (gnu_subprog_decl));

  begin_subprog_body (gnu_subprog_decl);
  gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);

  /* If there are OUT parameters, we need to ensure that the return statement
     properly copies them out.  We do this by making a new block and converting
     any inner return into a goto to a label at the end of the block.  */
  push_stack (&gnu_return_label_stack, NULL_TREE,
	      gnu_cico_list ? create_artificial_label () : NULL_TREE);

  /* Get a tree corresponding to the code for the subprogram.  */
  start_stmt_group ();
  gnat_pushlevel ();

  /* See if there are any parameters for which we don't yet have GCC entities.
     These must be for OUT parameters for which we will be making VAR_DECL
     nodes here.  Fill them in to TYPE_CI_CO_LIST, which must contain the empty
     entry as well.  We can match up the entries because TYPE_CI_CO_LIST is in
     the order of the parameters.  */
  for (gnat_param = First_Formal (gnat_subprog_id);
       Present (gnat_param);
       gnat_param = Next_Formal_With_Extras (gnat_param))
    if (!present_gnu_tree (gnat_param))
      {
	/* Skip any entries that have been already filled in; they must
	   correspond to IN OUT parameters.  */
	for (; gnu_cico_list && TREE_VALUE (gnu_cico_list);
	     gnu_cico_list = TREE_CHAIN (gnu_cico_list))
	  ;

	/* Do any needed references for padded types.  */
	TREE_VALUE (gnu_cico_list)
	  = convert (TREE_TYPE (TREE_PURPOSE (gnu_cico_list)),
		     gnat_to_gnu_entity (gnat_param, NULL_TREE, 1));
      }

  process_decls (Declarations (gnat_node), Empty, Empty, true, true);

  /* Generate the code of the subprogram itself.  A return statement will be
     present and any OUT parameters will be handled there.  */
  add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));
  gnat_poplevel ();
  gnu_result = end_stmt_group ();

  /* If we made a special return label, we need to make a block that contains
     the definition of that label and the copying to the return value.  That
     block first contains the function, then the label and copy statement.  */
  if (TREE_VALUE (gnu_return_label_stack))
    {
      tree gnu_retval;

      start_stmt_group ();
      gnat_pushlevel ();
      add_stmt (gnu_result);
      add_stmt (build1 (LABEL_EXPR, void_type_node,
			TREE_VALUE (gnu_return_label_stack)));

      gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);
      if (list_length (gnu_cico_list) == 1)
	gnu_retval = TREE_VALUE (gnu_cico_list);
      else
	gnu_retval = gnat_build_constructor (TREE_TYPE (gnu_subprog_type),
					     gnu_cico_list);

      if (DECL_P (gnu_retval) && DECL_BY_REF_P (gnu_retval))
	gnu_retval = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_retval);

      add_stmt_with_node
	(build_return_expr (DECL_RESULT (current_function_decl), gnu_retval),
	 gnat_node);
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
    }

  pop_stack (&gnu_return_label_stack);

  /* Initialize the information node for the function and set the
     end location.  */
  allocate_struct_function (current_function_decl);
  Sloc_to_locus
    ((Present (End_Label (Handled_Statement_Sequence (gnat_node)))
      ? Sloc (End_Label (Handled_Statement_Sequence (gnat_node)))
      : Sloc (gnat_node)),
     &cfun->function_end_locus);

  end_subprog_body (gnu_result);

  /* Disconnect the trees for parameters that we made variables for from the
     GNAT entities since these are unusable after we end the function.  */
  for (gnat_param = First_Formal (gnat_subprog_id);
       Present (gnat_param);
       gnat_param = Next_Formal_With_Extras (gnat_param))
    if (TREE_CODE (get_gnu_tree (gnat_param)) == VAR_DECL)
      save_gnu_tree (gnat_param, NULL_TREE, false);

  mark_out_of_scope (Defining_Unit_Name (Specification (gnat_node)));
  write_symbols = save_write_symbols;
  debug_hooks = save_debug_hooks;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, either an N_Function_Call
   or an N_Procedure_Call_Statement, to a GCC tree, which is returned.
   GNU_RESULT_TYPE_P is a pointer to where we should place the result type.
   If GNU_TARGET is non-null, this must be a function call and the result
   of the call is to be placed into that object.  */

static tree
call_to_gnu (Node_Id gnat_node, tree *gnu_result_type_p, tree gnu_target)
{
  tree gnu_result;
  /* The GCC node corresponding to the GNAT subprogram name.  This can either
     be a FUNCTION_DECL node if we are dealing with a standard subprogram call,
     or an indirect reference expression (an INDIRECT_REF node) pointing to a
     subprogram.  */
  tree gnu_subprog_node = gnat_to_gnu (Name (gnat_node));
  /* The FUNCTION_TYPE node giving the GCC type of the subprogram.  */
  tree gnu_subprog_type = TREE_TYPE (gnu_subprog_node);
  tree gnu_subprog_addr = build_unary_op (ADDR_EXPR, NULL_TREE,
					  gnu_subprog_node);
  Entity_Id gnat_formal;
  Node_Id gnat_actual;
  tree gnu_actual_list = NULL_TREE;
  tree gnu_name_list = NULL_TREE;
  tree gnu_before_list = NULL_TREE;
  tree gnu_after_list = NULL_TREE;
  tree gnu_subprog_call;

  switch (Nkind (Name (gnat_node)))
    {
    case N_Identifier:
    case N_Operator_Symbol:
    case N_Expanded_Name:
    case N_Attribute_Reference:
      if (Is_Eliminated (Entity (Name (gnat_node))))
	Eliminate_Error_Msg (gnat_node, Entity (Name (gnat_node)));
    }

  gcc_assert (TREE_CODE (gnu_subprog_type) == FUNCTION_TYPE);

  /* If we are calling a stubbed function, make this into a raise of
     Program_Error.  Elaborate all our args first.  */
  if (TREE_CODE (gnu_subprog_node) == FUNCTION_DECL
      && DECL_STUBBED_P (gnu_subprog_node))
    {
      for (gnat_actual = First_Actual (gnat_node);
	   Present (gnat_actual);
	   gnat_actual = Next_Actual (gnat_actual))
	add_stmt (gnat_to_gnu (gnat_actual));

      {
	tree call_expr
	  = build_call_raise (PE_Stubbed_Subprogram_Called, gnat_node);

	if (Nkind (gnat_node) == N_Function_Call && !gnu_target)
	  {
	    *gnu_result_type_p = TREE_TYPE (gnu_subprog_type);
	    return build1 (NULL_EXPR, *gnu_result_type_p, call_expr);
	  }
	else
	  return call_expr;
      }
    }

  /* If we are calling by supplying a pointer to a target, set up that
     pointer as the first argument.  Use GNU_TARGET if one was passed;
     otherwise, make a target by building a variable of the maximum size
     of the type.  */
  if (TYPE_RETURNS_BY_TARGET_PTR_P (gnu_subprog_type))
    {
      tree gnu_real_ret_type
	= TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (gnu_subprog_type)));

      if (!gnu_target)
	{
	  tree gnu_obj_type
	    = maybe_pad_type (gnu_real_ret_type,
			      max_size (TYPE_SIZE (gnu_real_ret_type), true),
			      0, Etype (Name (gnat_node)), "PAD", false,
			      false, false);

	  /* ??? We may be about to create a static temporary if we happen to
	     be at the global binding level.  That's a regression from what
	     the 3.x back-end would generate in the same situation, but we
	     don't have a mechanism in Gigi for creating automatic variables
	     in the elaboration routines.  */
	  gnu_target
	    = create_var_decl (create_tmp_var_name ("LR"), NULL, gnu_obj_type,
			       NULL, false, false, false, false, NULL,
			       gnat_node);
	}

      gnu_actual_list
	= tree_cons (NULL_TREE,
		     build_unary_op (ADDR_EXPR, NULL_TREE,
				     unchecked_convert (gnu_real_ret_type,
							gnu_target,
							false)),
		     NULL_TREE);

    }

  /* The only way we can be making a call via an access type is if Name is an
     explicit dereference.  In that case, get the list of formal args from the
     type the access type is pointing to.  Otherwise, get the formals from
     entity being called.  */
  if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
    gnat_formal = First_Formal (Etype (Name (gnat_node)));
  else if (Nkind (Name (gnat_node)) == N_Attribute_Reference)
    /* Assume here that this must be 'Elab_Body or 'Elab_Spec.  */
    gnat_formal = 0;
  else
    gnat_formal = First_Formal (Entity (Name (gnat_node)));

  /* Create the list of the actual parameters as GCC expects it, namely a chain
     of TREE_LIST nodes in which the TREE_VALUE field of each node is a
     parameter-expression and the TREE_PURPOSE field is null.  Skip OUT
     parameters not passed by reference and don't need to be copied in.  */
  for (gnat_actual = First_Actual (gnat_node);
       Present (gnat_actual);
       gnat_formal = Next_Formal_With_Extras (gnat_formal),
       gnat_actual = Next_Actual (gnat_actual))
    {
      tree gnu_formal
	= (present_gnu_tree (gnat_formal)
	   ? get_gnu_tree (gnat_formal) : NULL_TREE);
      tree gnu_formal_type = gnat_to_gnu_type (Etype (gnat_formal));
      /* We treat a conversion between aggregate types as if it is an
	 unchecked conversion.  */
      bool unchecked_convert_p
	= (Nkind (gnat_actual) == N_Unchecked_Type_Conversion
	   || (Nkind (gnat_actual) == N_Type_Conversion
	       && Is_Composite_Type (Underlying_Type (Etype (gnat_formal)))));
      Node_Id gnat_name = (unchecked_convert_p
			   ? Expression (gnat_actual) : gnat_actual);
      tree gnu_name = gnat_to_gnu (gnat_name);
      tree gnu_name_type = gnat_to_gnu_type (Etype (gnat_name));
      tree gnu_actual;

      /* If it's possible we may need to use this expression twice, make sure
	 than any side-effects are handled via SAVE_EXPRs. Likewise if we need
	 to force side-effects before the call.

	 ??? This is more conservative than we need since we don't need to do
	 this for pass-by-ref with no conversion. If we are passing a
	 non-addressable Out or In Out parameter by reference, pass the address
	 of a copy and set up to copy back out after the call.  */
      if (Ekind (gnat_formal) != E_In_Parameter)
	{
	  gnu_name = gnat_stabilize_reference (gnu_name, true);

	  if (!addressable_p (gnu_name)
	      && gnu_formal
	      && (DECL_BY_REF_P (gnu_formal)
		  || (TREE_CODE (gnu_formal) == PARM_DECL
		      && (DECL_BY_COMPONENT_PTR_P (gnu_formal)
			  || (DECL_BY_DESCRIPTOR_P (gnu_formal))))))
	    {
	      tree gnu_copy = gnu_name;
	      tree gnu_temp;

	      /* For users of Starlet we issue a warning because the
		 interface apparently assumes that by-ref parameters
		 outlive the procedure invocation.  The code still
		 will not work as intended, but we cannot do much
		 better since other low-level parts of the back-end
		 would allocate temporaries at will because of the
		 misalignment if we did not do so here.  */

	      if (Is_Valued_Procedure (Entity (Name (gnat_node))))
		{
		  post_error
		    ("?possible violation of implicit assumption",
		     gnat_actual);
		  post_error_ne
		    ("?made by pragma Import_Valued_Procedure on &",
		     gnat_actual, Entity (Name (gnat_node)));
		  post_error_ne
		    ("?because of misalignment of &",
		     gnat_actual, gnat_formal);
		}

	      /* Remove any unpadding on the actual and make a copy.  But if
		 the actual is a justified modular type, first convert
		 to it.  */
	      if (TREE_CODE (gnu_name) == COMPONENT_REF
		  && ((TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_name, 0)))
		       == RECORD_TYPE)
		      && (TYPE_IS_PADDING_P
			  (TREE_TYPE (TREE_OPERAND (gnu_name, 0))))))
		gnu_name = gnu_copy = TREE_OPERAND (gnu_name, 0);
	      else if (TREE_CODE (gnu_name_type) == RECORD_TYPE
		       && (TYPE_JUSTIFIED_MODULAR_P (gnu_name_type)))
		gnu_name = convert (gnu_name_type, gnu_name);

	      gnu_actual = save_expr (gnu_name);

	      /* Since we're going to take the address of the SAVE_EXPR, we
		 don't want it to be marked as unchanging. So set
		 TREE_ADDRESSABLE.  */
	      gnu_temp = skip_simple_arithmetic (gnu_actual);
	      if (TREE_CODE (gnu_temp) == SAVE_EXPR)
		{
		  TREE_ADDRESSABLE (gnu_temp) = 1;
		  TREE_READONLY (gnu_temp) = 0;
		}

	      /* Set up to move the copy back to the original.  */
	      gnu_temp = build_binary_op (MODIFY_EXPR, NULL_TREE,
					  gnu_copy, gnu_actual);
	      annotate_with_node (gnu_temp, gnat_actual);
	      append_to_statement_list (gnu_temp, &gnu_after_list);

	      /* Account for next statement just below.  */
	      gnu_name = gnu_actual;
	    }
	}

      /* If this was a procedure call, we may not have removed any padding.
	 So do it here for the part we will use as an input, if any.  */
      gnu_actual = gnu_name;
      if (Ekind (gnat_formal) != E_Out_Parameter
	  && TREE_CODE (TREE_TYPE (gnu_actual)) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_actual)))
	gnu_actual = convert (get_unpadded_type (Etype (gnat_actual)),
			      gnu_actual);

      /* Unless this is an In parameter, we must remove any LJM building
	 from GNU_NAME.  */
      if (Ekind (gnat_formal) != E_In_Parameter
	  && TREE_CODE (gnu_name) == CONSTRUCTOR
	  && TREE_CODE (TREE_TYPE (gnu_name)) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (TREE_TYPE (gnu_name)))
	gnu_name = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_name))),
			    gnu_name);

      if (Ekind (gnat_formal) != E_Out_Parameter
	  && !unchecked_convert_p
	  && Do_Range_Check (gnat_actual))
	gnu_actual = emit_range_check (gnu_actual, Etype (gnat_formal));

      /* Do any needed conversions.  We need only check for unchecked
	 conversion since normal conversions will be handled by just
	 converting to the formal type.  */
      if (unchecked_convert_p)
	{
	  gnu_actual
	    = unchecked_convert (gnat_to_gnu_type (Etype (gnat_actual)),
				 gnu_actual,
				 (Nkind (gnat_actual)
				  == N_Unchecked_Type_Conversion)
				 && No_Truncation (gnat_actual));

	  /* One we've done the unchecked conversion, we still must ensure that
	     the object is in range of the formal's type.  */
	  if (Ekind (gnat_formal) != E_Out_Parameter
	      && Do_Range_Check (gnat_actual))
	    gnu_actual = emit_range_check (gnu_actual,
					   Etype (gnat_formal));
	}
      else if (TREE_CODE (gnu_actual) != SAVE_EXPR)
	/* We may have suppressed a conversion to the Etype of the actual since
	   the parent is a procedure call.  So add the conversion here.  */
	gnu_actual = convert (gnat_to_gnu_type (Etype (gnat_actual)),
			      gnu_actual);

      if (TREE_CODE (gnu_actual) != SAVE_EXPR)
	gnu_actual = convert (gnu_formal_type, gnu_actual);

      /* If we have not saved a GCC object for the formal, it means it is an
	 OUT parameter not passed by reference and that does not need to be
	 copied in. Otherwise, look at the PARM_DECL to see if it is passed by
	 reference. */
      if (gnu_formal
	  && TREE_CODE (gnu_formal) == PARM_DECL && DECL_BY_REF_P (gnu_formal))
	{
	  if (Ekind (gnat_formal) != E_In_Parameter)
	    {
	      gnu_actual = gnu_name;

	      /* If we have a padded type, be sure we've removed padding.  */
	      if (TREE_CODE (TREE_TYPE (gnu_actual)) == RECORD_TYPE
		  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_actual))
		  && TREE_CODE (gnu_actual) != SAVE_EXPR)
		gnu_actual = convert (get_unpadded_type (Etype (gnat_actual)),
				      gnu_actual);

	      /* If we have the constructed subtype of an aliased object
		 with an unconstrained nominal subtype, the type of the
		 actual includes the template, although it is formally
		 constrained.  So we need to convert it back to the real
		 constructed subtype to retrieve the constrained part
		 and takes its address.  */
	      if (TREE_CODE (TREE_TYPE (gnu_actual)) == RECORD_TYPE
		  && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (gnu_actual))
		  && TREE_CODE (gnu_actual) != SAVE_EXPR
		  && Is_Constr_Subt_For_UN_Aliased (Etype (gnat_actual))
		  && Is_Array_Type (Etype (gnat_actual)))
		gnu_actual = convert (gnat_to_gnu_type (Etype (gnat_actual)),
				      gnu_actual);
	    }

	  /* Otherwise, if we have a non-addressable COMPONENT_REF of a
	     variable-size type see if it's doing a unpadding operation.  If
	     so, remove that operation since we have no way of allocating the
	     required temporary.  */
	  if (TREE_CODE (gnu_actual) == COMPONENT_REF
	      && !TREE_CONSTANT (TYPE_SIZE (TREE_TYPE (gnu_actual)))
	      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_actual, 0)))
		  == RECORD_TYPE)
	      && TYPE_IS_PADDING_P (TREE_TYPE
				    (TREE_OPERAND (gnu_actual, 0)))
	      && !addressable_p (gnu_actual))
	    gnu_actual = TREE_OPERAND (gnu_actual, 0);

	  /* The symmetry of the paths to the type of an entity is broken here
	     since arguments don't know that they will be passed by ref. */
	  gnu_formal_type = TREE_TYPE (get_gnu_tree (gnat_formal));
	  gnu_actual = build_unary_op (ADDR_EXPR, gnu_formal_type, gnu_actual);
	}
      else if (gnu_formal && TREE_CODE (gnu_formal) == PARM_DECL
	       && DECL_BY_COMPONENT_PTR_P (gnu_formal))
	{
	  gnu_formal_type = TREE_TYPE (get_gnu_tree (gnat_formal));
	  gnu_actual = maybe_implicit_deref (gnu_actual);
	  gnu_actual = maybe_unconstrained_array (gnu_actual);

	  if (TREE_CODE (gnu_formal_type) == RECORD_TYPE
	      && TYPE_IS_PADDING_P (gnu_formal_type))
	    {
	      gnu_formal_type = TREE_TYPE (TYPE_FIELDS (gnu_formal_type));
	      gnu_actual = convert (gnu_formal_type, gnu_actual);
	    }

	  /* Take the address of the object and convert to the proper pointer
	     type.  We'd like to actually compute the address of the beginning
	     of the array using an ADDR_EXPR of an ARRAY_REF, but there's a
	     possibility that the ARRAY_REF might return a constant and we'd be
	     getting the wrong address.  Neither approach is exactly correct,
	     but this is the most likely to work in all cases.  */
	  gnu_actual = convert (gnu_formal_type,
				build_unary_op (ADDR_EXPR, NULL_TREE,
						gnu_actual));
	}
      else if (gnu_formal && TREE_CODE (gnu_formal) == PARM_DECL
	       && DECL_BY_DESCRIPTOR_P (gnu_formal))
	{
	  /* If arg is 'Null_Parameter, pass zero descriptor.  */
	  if ((TREE_CODE (gnu_actual) == INDIRECT_REF
	       || TREE_CODE (gnu_actual) == UNCONSTRAINED_ARRAY_REF)
	      && TREE_PRIVATE (gnu_actual))
	    gnu_actual = convert (DECL_ARG_TYPE (get_gnu_tree (gnat_formal)),
				  integer_zero_node);
	  else
	    gnu_actual = build_unary_op (ADDR_EXPR, NULL_TREE,
					 fill_vms_descriptor (gnu_actual,
							      gnat_formal));
	}
      else
	{
	  tree gnu_actual_size = TYPE_SIZE (TREE_TYPE (gnu_actual));

	  if (Ekind (gnat_formal) != E_In_Parameter)
	    gnu_name_list = tree_cons (NULL_TREE, gnu_name, gnu_name_list);

	  if (!gnu_formal || TREE_CODE (gnu_formal) != PARM_DECL)
	    continue;

	  /* If this is 'Null_Parameter, pass a zero even though we are
	     dereferencing it.  */
	  else if (TREE_CODE (gnu_actual) == INDIRECT_REF
		   && TREE_PRIVATE (gnu_actual)
		   && host_integerp (gnu_actual_size, 1)
		   && 0 >= compare_tree_int (gnu_actual_size,
						   BITS_PER_WORD))
	    gnu_actual
	      = unchecked_convert (DECL_ARG_TYPE (gnu_formal),
				   convert (gnat_type_for_size
					    (tree_low_cst (gnu_actual_size, 1),
					     1),
					    integer_zero_node),
				   false);
	  else
	    gnu_actual = convert (DECL_ARG_TYPE (gnu_formal), gnu_actual);
	}

      gnu_actual_list = tree_cons (NULL_TREE, gnu_actual, gnu_actual_list);
    }

  gnu_subprog_call = build3 (CALL_EXPR, TREE_TYPE (gnu_subprog_type),
			     gnu_subprog_addr, nreverse (gnu_actual_list),
			     NULL_TREE);

  /* If we return by passing a target, we emit the call and return the target
     as our result.  */
  if (TYPE_RETURNS_BY_TARGET_PTR_P (gnu_subprog_type))
    {
      add_stmt_with_node (gnu_subprog_call, gnat_node);
      *gnu_result_type_p
	= TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (gnu_subprog_type)));
      return unchecked_convert (*gnu_result_type_p, gnu_target, false);
    }

  /* If it is a function call, the result is the call expression unless
     a target is specified, in which case we copy the result into the target
     and return the assignment statement.  */
  else if (Nkind (gnat_node) == N_Function_Call)
    {
      gnu_result = gnu_subprog_call;

      /* If the function returns an unconstrained array or by reference,
	 we have to de-dereference the pointer.  */
      if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type)
	  || TYPE_RETURNS_BY_REF_P (gnu_subprog_type))
	gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);

      if (gnu_target)
	gnu_result = build_binary_op (MODIFY_EXPR, NULL_TREE,
				      gnu_target, gnu_result);
      else
	*gnu_result_type_p = get_unpadded_type (Etype (gnat_node));

      return gnu_result;
    }

  /* If this is the case where the GNAT tree contains a procedure call
     but the Ada procedure has copy in copy out parameters, the special
     parameter passing mechanism must be used.  */
  else if (TYPE_CI_CO_LIST (gnu_subprog_type) != NULL_TREE)
    {
      /* List of FIELD_DECLs associated with the PARM_DECLs of the copy
	 in copy out parameters.  */
      tree scalar_return_list = TYPE_CI_CO_LIST (gnu_subprog_type);
      int length = list_length (scalar_return_list);

      if (length > 1)
	{
	  tree gnu_name;

	  gnu_subprog_call = save_expr (gnu_subprog_call);
	  gnu_name_list = nreverse (gnu_name_list);

	  /* If any of the names had side-effects, ensure they are all
	     evaluated before the call.  */
	  for (gnu_name = gnu_name_list; gnu_name;
	       gnu_name = TREE_CHAIN (gnu_name))
	    if (TREE_SIDE_EFFECTS (TREE_VALUE (gnu_name)))
	      append_to_statement_list (TREE_VALUE (gnu_name),
					&gnu_before_list);
	}

      if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	gnat_formal = First_Formal (Etype (Name (gnat_node)));
      else
	gnat_formal = First_Formal (Entity (Name (gnat_node)));

      for (gnat_actual = First_Actual (gnat_node);
	   Present (gnat_actual);
	   gnat_formal = Next_Formal_With_Extras (gnat_formal),
	   gnat_actual = Next_Actual (gnat_actual))
	/* If we are dealing with a copy in copy out parameter, we must
	   retrieve its value from the record returned in the call.  */
	if (!(present_gnu_tree (gnat_formal)
	      && TREE_CODE (get_gnu_tree (gnat_formal)) == PARM_DECL
	      && (DECL_BY_REF_P (get_gnu_tree (gnat_formal))
		  || (TREE_CODE (get_gnu_tree (gnat_formal)) == PARM_DECL
		      && ((DECL_BY_COMPONENT_PTR_P (get_gnu_tree (gnat_formal))
			   || (DECL_BY_DESCRIPTOR_P
			       (get_gnu_tree (gnat_formal))))))))
	    && Ekind (gnat_formal) != E_In_Parameter)
	  {
	    /* Get the value to assign to this OUT or IN OUT parameter.  It is
	       either the result of the function if there is only a single such
	       parameter or the appropriate field from the record returned.  */
	    tree gnu_result
	      = length == 1 ? gnu_subprog_call
		: build_component_ref (gnu_subprog_call, NULL_TREE,
				       TREE_PURPOSE (scalar_return_list),
				       false);
	    bool unchecked_conversion = (Nkind (gnat_actual)
					 == N_Unchecked_Type_Conversion);
	    /* If the actual is a conversion, get the inner expression, which
	       will be the real destination, and convert the result to the
	       type of the actual parameter.  */
	    tree gnu_actual
	      = maybe_unconstrained_array (TREE_VALUE (gnu_name_list));

	    /* If the result is a padded type, remove the padding.  */
	    if (TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
		&& TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
	      gnu_result = convert (TREE_TYPE (TYPE_FIELDS
					       (TREE_TYPE (gnu_result))),
				    gnu_result);

	    /* If the result is a type conversion, do it.  */
	    if (Nkind (gnat_actual) == N_Type_Conversion)
	      gnu_result
		= convert_with_check
		  (Etype (Expression (gnat_actual)), gnu_result,
		   Do_Overflow_Check (gnat_actual),
		   Do_Range_Check (Expression (gnat_actual)),
		   Float_Truncate (gnat_actual));

	    else if (unchecked_conversion)
	      gnu_result = unchecked_convert (TREE_TYPE (gnu_actual),
					      gnu_result,
					      No_Truncation (gnat_actual));
	    else
	      {
		if (Do_Range_Check (gnat_actual))
		  gnu_result = emit_range_check (gnu_result,
						 Etype (gnat_actual));

		if (!(!TREE_CONSTANT (TYPE_SIZE (TREE_TYPE (gnu_actual)))
		      && TREE_CONSTANT (TYPE_SIZE (TREE_TYPE (gnu_result)))))
		  gnu_result = convert (TREE_TYPE (gnu_actual), gnu_result);
	      }

	    gnu_result = build_binary_op (MODIFY_EXPR, NULL_TREE,
					  gnu_actual, gnu_result);
	    annotate_with_node (gnu_result, gnat_actual);
	    append_to_statement_list (gnu_result, &gnu_before_list);
	    scalar_return_list = TREE_CHAIN (scalar_return_list);
	    gnu_name_list = TREE_CHAIN (gnu_name_list);
	  }
	}
  else
    {
      annotate_with_node (gnu_subprog_call, gnat_node);
      append_to_statement_list (gnu_subprog_call, &gnu_before_list);
    }

  append_to_statement_list (gnu_after_list, &gnu_before_list);
  return gnu_before_list;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an
   N_Handled_Sequence_Of_Statements, to a GCC tree, which is returned.  */

static tree
Handled_Sequence_Of_Statements_to_gnu (Node_Id gnat_node)
{
  tree gnu_jmpsave_decl = NULL_TREE;
  tree gnu_jmpbuf_decl = NULL_TREE;
  /* If just annotating, ignore all EH and cleanups.  */
  bool gcc_zcx = (!type_annotate_only
		  && Present (Exception_Handlers (gnat_node))
		  && Exception_Mechanism == Back_End_Exceptions);
  bool setjmp_longjmp
    = (!type_annotate_only && Present (Exception_Handlers (gnat_node))
       && Exception_Mechanism == Setjmp_Longjmp);
  bool at_end = !type_annotate_only && Present (At_End_Proc (gnat_node));
  bool binding_for_block = (at_end || gcc_zcx || setjmp_longjmp);
  tree gnu_inner_block; /* The statement(s) for the block itself.  */
  tree gnu_result;
  tree gnu_expr;
  Node_Id gnat_temp;

  /* The GCC exception handling mechanism can handle both ZCX and SJLJ schemes
     and we have our own SJLJ mechanism.  To call the GCC mechanism, we call
     add_cleanup, and when we leave the binding, end_stmt_group will create
     the TRY_FINALLY_EXPR.

     ??? The region level calls down there have been specifically put in place
     for a ZCX context and currently the order in which things are emitted
     (region/handlers) is different from the SJLJ case. Instead of putting
     other calls with different conditions at other places for the SJLJ case,
     it seems cleaner to reorder things for the SJLJ case and generalize the
     condition to make it not ZCX specific.

     If there are any exceptions or cleanup processing involved, we need an
     outer statement group (for Setjmp_Longjmp) and binding level.  */
  if (binding_for_block)
    {
      start_stmt_group ();
      gnat_pushlevel ();
    }

  /* If we are to call a function when exiting this block add a cleanup
     to the binding level we made above.  */
  if (at_end)
    add_cleanup (build_call_0_expr (gnat_to_gnu (At_End_Proc (gnat_node))));

  /* If using setjmp_longjmp, make the variables for the setjmp buffer and save
     area for address of previous buffer.  Do this first since we need to have
     the setjmp buf known for any decls in this block.  */
  if (setjmp_longjmp)
    {
      gnu_jmpsave_decl = create_var_decl (get_identifier ("JMPBUF_SAVE"),
					  NULL_TREE, jmpbuf_ptr_type,
					  build_call_0_expr (get_jmpbuf_decl),
					  false, false, false, false, NULL,
					  gnat_node);
      gnu_jmpbuf_decl = create_var_decl (get_identifier ("JMP_BUF"),
					 NULL_TREE, jmpbuf_type,
					 NULL_TREE, false, false, false, false,
					 NULL, gnat_node);

      set_block_jmpbuf_decl (gnu_jmpbuf_decl);

      /* When we exit this block, restore the saved value.  */
      add_cleanup (build_call_1_expr (set_jmpbuf_decl, gnu_jmpsave_decl));
    }

  /* Now build the tree for the declarations and statements inside this block.
     If this is SJLJ, set our jmp_buf as the current buffer.  */
  start_stmt_group ();

  if (setjmp_longjmp)
    add_stmt (build_call_1_expr (set_jmpbuf_decl,
				 build_unary_op (ADDR_EXPR, NULL_TREE,
						 gnu_jmpbuf_decl)));

  if (Present (First_Real_Statement (gnat_node)))
    process_decls (Statements (gnat_node), Empty,
		   First_Real_Statement (gnat_node), true, true);

  /* Generate code for each statement in the block.  */
  for (gnat_temp = (Present (First_Real_Statement (gnat_node))
		    ? First_Real_Statement (gnat_node)
		    : First (Statements (gnat_node)));
       Present (gnat_temp); gnat_temp = Next (gnat_temp))
    add_stmt (gnat_to_gnu (gnat_temp));
  gnu_inner_block = end_stmt_group ();

  /* Now generate code for the two exception models, if either is relevant for
     this block.  */
  if (setjmp_longjmp)
    {
      tree *gnu_else_ptr = 0;
      tree gnu_handler;

      /* Make a binding level for the exception handling declarations and code
	 and set up gnu_except_ptr_stack for the handlers to use.  */
      start_stmt_group ();
      gnat_pushlevel ();

      push_stack (&gnu_except_ptr_stack, NULL_TREE,
		  create_var_decl (get_identifier ("EXCEPT_PTR"),
				   NULL_TREE,
				   build_pointer_type (except_type_node),
				   build_call_0_expr (get_excptr_decl), false,
				   false, false, false, NULL, gnat_node));

      /* Generate code for each handler. The N_Exception_Handler case does the
	 real work and returns a COND_EXPR for each handler, which we chain
	 together here.  */
      for (gnat_temp = First_Non_Pragma (Exception_Handlers (gnat_node));
	   Present (gnat_temp); gnat_temp = Next_Non_Pragma (gnat_temp))
	{
	  gnu_expr = gnat_to_gnu (gnat_temp);

	  /* If this is the first one, set it as the outer one. Otherwise,
	     point the "else" part of the previous handler to us. Then point
	     to our "else" part.  */
	  if (!gnu_else_ptr)
	    add_stmt (gnu_expr);
	  else
	    *gnu_else_ptr = gnu_expr;

	  gnu_else_ptr = &COND_EXPR_ELSE (gnu_expr);
	}

      /* If none of the exception handlers did anything, re-raise but do not
	 defer abortion.  */
      gnu_expr = build_call_1_expr (raise_nodefer_decl,
				    TREE_VALUE (gnu_except_ptr_stack));
      annotate_with_node (gnu_expr, gnat_node);

      if (gnu_else_ptr)
	*gnu_else_ptr = gnu_expr;
      else
	add_stmt (gnu_expr);

      /* End the binding level dedicated to the exception handlers and get the
	 whole statement group.  */
      pop_stack (&gnu_except_ptr_stack);
      gnat_poplevel ();
      gnu_handler = end_stmt_group ();

      /* If the setjmp returns 1, we restore our incoming longjmp value and
	 then check the handlers.  */
      start_stmt_group ();
      add_stmt_with_node (build_call_1_expr (set_jmpbuf_decl,
					     gnu_jmpsave_decl),
			  gnat_node);
      add_stmt (gnu_handler);
      gnu_handler = end_stmt_group ();

      /* This block is now "if (setjmp) ... <handlers> else <block>".  */
      gnu_result = build3 (COND_EXPR, void_type_node,
			   (build_call_1_expr
			    (setjmp_decl,
			     build_unary_op (ADDR_EXPR, NULL_TREE,
					     gnu_jmpbuf_decl))),
			   gnu_handler, gnu_inner_block);
    }
  else if (gcc_zcx)
    {
      tree gnu_handlers;

      /* First make a block containing the handlers.  */
      start_stmt_group ();
      for (gnat_temp = First_Non_Pragma (Exception_Handlers (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next_Non_Pragma (gnat_temp))
	add_stmt (gnat_to_gnu (gnat_temp));
      gnu_handlers = end_stmt_group ();

      /* Now make the TRY_CATCH_EXPR for the block.  */
      gnu_result = build2 (TRY_CATCH_EXPR, void_type_node,
			   gnu_inner_block, gnu_handlers);
    }
  else
    gnu_result = gnu_inner_block;

  /* Now close our outer block, if we had to make one.  */
  if (binding_for_block)
    {
      add_stmt (gnu_result);
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
    }

  return gnu_result;
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Exception_Handler,
   to a GCC tree, which is returned.  This is the variant for Setjmp_Longjmp
   exception handling.  */

static tree
Exception_Handler_to_gnu_sjlj (Node_Id gnat_node)
{
  /* Unless this is "Others" or the special "Non-Ada" exception for Ada, make
     an "if" statement to select the proper exceptions.  For "Others", exclude
     exceptions where Handled_By_Others is nonzero unless the All_Others flag
     is set. For "Non-ada", accept an exception if "Lang" is 'V'.  */
  tree gnu_choice = integer_zero_node;
  tree gnu_body = build_stmt_group (Statements (gnat_node), false);
  Node_Id gnat_temp;

  for (gnat_temp = First (Exception_Choices (gnat_node));
       gnat_temp; gnat_temp = Next (gnat_temp))
    {
      tree this_choice;

      if (Nkind (gnat_temp) == N_Others_Choice)
	{
	  if (All_Others (gnat_temp))
	    this_choice = integer_one_node;
	  else
	    this_choice
	      = build_binary_op
		(EQ_EXPR, integer_type_node,
		 convert
		 (integer_type_node,
		  build_component_ref
		  (build_unary_op
		   (INDIRECT_REF, NULL_TREE,
		    TREE_VALUE (gnu_except_ptr_stack)),
		   get_identifier ("not_handled_by_others"), NULL_TREE,
		   false)),
		 integer_zero_node);
	}

      else if (Nkind (gnat_temp) == N_Identifier
	       || Nkind (gnat_temp) == N_Expanded_Name)
	{
	  Entity_Id gnat_ex_id = Entity (gnat_temp);
	  tree gnu_expr;

	  /* Exception may be a renaming. Recover original exception which is
	     the one elaborated and registered.  */
	  if (Present (Renamed_Object (gnat_ex_id)))
	    gnat_ex_id = Renamed_Object (gnat_ex_id);

	  gnu_expr = gnat_to_gnu_entity (gnat_ex_id, NULL_TREE, 0);

	  this_choice
	    = build_binary_op
	      (EQ_EXPR, integer_type_node, TREE_VALUE (gnu_except_ptr_stack),
	       convert (TREE_TYPE (TREE_VALUE (gnu_except_ptr_stack)),
			build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr)));

	  /* If this is the distinguished exception "Non_Ada_Error" (and we are
	     in VMS mode), also allow a non-Ada exception (a VMS condition) t
	     match.  */
	  if (Is_Non_Ada_Error (Entity (gnat_temp)))
	    {
	      tree gnu_comp
		= build_component_ref
		  (build_unary_op (INDIRECT_REF, NULL_TREE,
				   TREE_VALUE (gnu_except_ptr_stack)),
		   get_identifier ("lang"), NULL_TREE, false);

	      this_choice
		= build_binary_op
		  (TRUTH_ORIF_EXPR, integer_type_node,
		   build_binary_op (EQ_EXPR, integer_type_node, gnu_comp,
				    build_int_cst (TREE_TYPE (gnu_comp), 'V')),
		   this_choice);
	    }
	}
      else
	gcc_unreachable ();

      gnu_choice = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
				    gnu_choice, this_choice);
    }

  return build3 (COND_EXPR, void_type_node, gnu_choice, gnu_body, NULL_TREE);
}

/* Subroutine of gnat_to_gnu to translate gnat_node, an N_Exception_Handler,
   to a GCC tree, which is returned.  This is the variant for ZCX.  */

static tree
Exception_Handler_to_gnu_zcx (Node_Id gnat_node)
{
  tree gnu_etypes_list = NULL_TREE;
  tree gnu_expr;
  tree gnu_etype;
  tree gnu_current_exc_ptr;
  tree gnu_incoming_exc_ptr;
  Node_Id gnat_temp;

  /* We build a TREE_LIST of nodes representing what exception types this
     handler can catch, with special cases for others and all others cases.

     Each exception type is actually identified by a pointer to the exception
     id, or to a dummy object for "others" and "all others".

     Care should be taken to ensure that the control flow impact of "others"
     and "all others" is known to GCC. lang_eh_type_covers is doing the trick
     currently.  */
  for (gnat_temp = First (Exception_Choices (gnat_node));
       gnat_temp; gnat_temp = Next (gnat_temp))
    {
      if (Nkind (gnat_temp) == N_Others_Choice)
	{
	  tree gnu_expr
	    = All_Others (gnat_temp) ? all_others_decl : others_decl;

	  gnu_etype
	    = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);
	}
      else if (Nkind (gnat_temp) == N_Identifier
	       || Nkind (gnat_temp) == N_Expanded_Name)
	{
	  Entity_Id gnat_ex_id = Entity (gnat_temp);

	  /* Exception may be a renaming. Recover original exception which is
	     the one elaborated and registered.  */
	  if (Present (Renamed_Object (gnat_ex_id)))
	    gnat_ex_id = Renamed_Object (gnat_ex_id);

	  gnu_expr = gnat_to_gnu_entity (gnat_ex_id, NULL_TREE, 0);
	  gnu_etype = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);

	  /* The Non_Ada_Error case for VMS exceptions is handled
	     by the personality routine.  */
	}
      else
	gcc_unreachable ();

      /* The GCC interface expects NULL to be passed for catch all handlers, so
	 it would be quite tempting to set gnu_etypes_list to NULL if gnu_etype
	 is integer_zero_node.  It would not work, however, because GCC's
	 notion of "catch all" is stronger than our notion of "others".  Until
	 we correctly use the cleanup interface as well, doing that would
	 prevent the "all others" handlers from being seen, because nothing
	 can be caught beyond a catch all from GCC's point of view.  */
      gnu_etypes_list = tree_cons (NULL_TREE, gnu_etype, gnu_etypes_list);
    }

  start_stmt_group ();
  gnat_pushlevel ();

  /* Expand a call to the begin_handler hook at the beginning of the handler,
     and arrange for a call to the end_handler hook to occur on every possible
     exit path.

     The hooks expect a pointer to the low level occurrence. This is required
     for our stack management scheme because a raise inside the handler pushes
     a new occurrence on top of the stack, which means that this top does not
     necessarily match the occurrence this handler was dealing with.

     The EXC_PTR_EXPR object references the exception occurrence being
     propagated. Upon handler entry, this is the exception for which the
     handler is triggered. This might not be the case upon handler exit,
     however, as we might have a new occurrence propagated by the handler's
     body, and the end_handler hook called as a cleanup in this context.

     We use a local variable to retrieve the incoming value at handler entry
     time, and reuse it to feed the end_handler hook's argument at exit.  */
  gnu_current_exc_ptr = build0 (EXC_PTR_EXPR, ptr_type_node);
  gnu_incoming_exc_ptr = create_var_decl (get_identifier ("EXPTR"), NULL_TREE,
					  ptr_type_node, gnu_current_exc_ptr,
					  false, false, false, false, NULL,
					  gnat_node);

  add_stmt_with_node (build_call_1_expr (begin_handler_decl,
					 gnu_incoming_exc_ptr),
		      gnat_node);
  add_cleanup (build_call_1_expr (end_handler_decl, gnu_incoming_exc_ptr));
  add_stmt_list (Statements (gnat_node));
  gnat_poplevel ();

  return build2 (CATCH_EXPR, void_type_node, gnu_etypes_list,
		 end_stmt_group ());
}

/* Subroutine of gnat_to_gnu to generate code for an N_Compilation unit.  */

static void
Compilation_Unit_to_gnu (Node_Id gnat_node)
{
  /* Make the decl for the elaboration procedure.  */
  bool body_p = (Defining_Entity (Unit (gnat_node)),
	    Nkind (Unit (gnat_node)) == N_Package_Body
	    || Nkind (Unit (gnat_node)) == N_Subprogram_Body);
  Entity_Id gnat_unit_entity = Defining_Entity (Unit (gnat_node));
  tree gnu_elab_proc_decl
    = create_subprog_decl
      (create_concat_name (gnat_unit_entity,
			   body_p ? "elabb" : "elabs"),
       NULL_TREE, void_ftype, NULL_TREE, false, true, false, NULL,
       gnat_unit_entity);
  struct elab_info *info;

  push_stack (&gnu_elab_proc_stack, NULL_TREE, gnu_elab_proc_decl);

  DECL_ELABORATION_PROC_P (gnu_elab_proc_decl) = 1;
  allocate_struct_function (gnu_elab_proc_decl);
  Sloc_to_locus (Sloc (gnat_unit_entity), &cfun->function_end_locus);
  cfun = 0;

  /* For a body, first process the spec if there is one. */
  if (Nkind (Unit (gnat_node)) == N_Package_Body
      || (Nkind (Unit (gnat_node)) == N_Subprogram_Body
	      && !Acts_As_Spec (gnat_node)))
    add_stmt (gnat_to_gnu (Library_Unit (gnat_node)));

  process_inlined_subprograms (gnat_node);

  if (type_annotate_only)
    {
      elaborate_all_entities (gnat_node);

      if (Nkind (Unit (gnat_node)) == N_Subprogram_Declaration
	  || Nkind (Unit (gnat_node)) == N_Generic_Package_Declaration
	  || Nkind (Unit (gnat_node)) == N_Generic_Subprogram_Declaration)
	return;
    }

  process_decls (Declarations (Aux_Decls_Node (gnat_node)), Empty, Empty,
		 true, true);
  add_stmt (gnat_to_gnu (Unit (gnat_node)));

  /* Process any pragmas and actions following the unit.  */
  add_stmt_list (Pragmas_After (Aux_Decls_Node (gnat_node)));
  add_stmt_list (Actions (Aux_Decls_Node (gnat_node)));

  /* Save away what we've made so far and record this potential elaboration
     procedure.  */
  info = (struct elab_info *) ggc_alloc (sizeof (struct elab_info));
  set_current_block_context (gnu_elab_proc_decl);
  gnat_poplevel ();
  DECL_SAVED_TREE (gnu_elab_proc_decl) = end_stmt_group ();
  info->next = elab_info_list;
  info->elab_proc = gnu_elab_proc_decl;
  info->gnat_node = gnat_node;
  elab_info_list = info;

  /* Generate elaboration code for this unit, if necessary, and say whether
     we did or not.  */
  pop_stack (&gnu_elab_proc_stack);

  /* Generate functions to call static constructors and destructors
     for targets that do not support .ctors/.dtors sections.  These
     functions have magic names which are detected by collect2.  */
  if (static_ctors)
    build_global_cdtor ('I', &static_ctors);

  if (static_dtors)
    build_global_cdtor ('D', &static_dtors);
}

/* This function is the driver of the GNAT to GCC tree transformation
   process.  It is the entry point of the tree transformer.  GNAT_NODE is the
   root of some GNAT tree.  Return the root of the corresponding GCC tree.
   If this is an expression, return the GCC equivalent of the expression.  If
   it is a statement, return the statement.  In the case when called for a
   statement, it may also add statements to the current statement group, in
   which case anything it returns is to be interpreted as occurring after
   anything `it already added.  */

tree
gnat_to_gnu (Node_Id gnat_node)
{
  bool went_into_elab_proc = false;
  tree gnu_result = error_mark_node; /* Default to no value. */
  tree gnu_result_type = void_type_node;
  tree gnu_expr;
  tree gnu_lhs, gnu_rhs;
  Node_Id gnat_temp;

  /* Save node number for error message and set location information.  */
  error_gnat_node = gnat_node;
  Sloc_to_locus (Sloc (gnat_node), &input_location);

  if (type_annotate_only
      && IN (Nkind (gnat_node), N_Statement_Other_Than_Procedure_Call))
    return alloc_stmt_list ();

  /* If this node is a non-static subexpression and we are only
     annotating types, make this into a NULL_EXPR.  */
  if (type_annotate_only
      && IN (Nkind (gnat_node), N_Subexpr)
      && Nkind (gnat_node) != N_Identifier
      && !Compile_Time_Known_Value (gnat_node))
    return build1 (NULL_EXPR, get_unpadded_type (Etype (gnat_node)),
		   build_call_raise (CE_Range_Check_Failed, gnat_node));

  /* If this is a Statement and we are at top level, it must be part of the
     elaboration procedure, so mark us as being in that procedure and push our
     context.

     If we are in the elaboration procedure, check if we are violating a a
     No_Elaboration_Code restriction by having a statement there.  */
  if ((IN (Nkind (gnat_node), N_Statement_Other_Than_Procedure_Call)
       && Nkind (gnat_node) != N_Null_Statement)
      || Nkind (gnat_node) == N_Procedure_Call_Statement
      || Nkind (gnat_node) == N_Label
      || Nkind (gnat_node) == N_Implicit_Label_Declaration
      || Nkind (gnat_node) == N_Handled_Sequence_Of_Statements
      || ((Nkind (gnat_node) == N_Raise_Constraint_Error
	   || Nkind (gnat_node) == N_Raise_Storage_Error
	   || Nkind (gnat_node) == N_Raise_Program_Error)
	  && (Ekind (Etype (gnat_node)) == E_Void)))
    {
      if (!current_function_decl)
	{
	  current_function_decl = TREE_VALUE (gnu_elab_proc_stack);
	  start_stmt_group ();
	  gnat_pushlevel ();
	  went_into_elab_proc = true;
	}

      /* Don't check for a possible No_Elaboration_Code restriction violation
	 on N_Handled_Sequence_Of_Statements, as we want to signal an error on
	 every nested real statement instead.  This also avoids triggering
	 spurious errors on dummy (empty) sequences created by the front-end
	 for package bodies in some cases.  */

      if (current_function_decl == TREE_VALUE (gnu_elab_proc_stack)
	  && Nkind (gnat_node) != N_Handled_Sequence_Of_Statements)
	Check_Elaboration_Code_Allowed (gnat_node);
    }

  switch (Nkind (gnat_node))
    {
      /********************************/
      /* Chapter 2: Lexical Elements: */
      /********************************/

    case N_Identifier:
    case N_Expanded_Name:
    case N_Operator_Symbol:
    case N_Defining_Identifier:
      gnu_result = Identifier_to_gnu (gnat_node, &gnu_result_type);
      break;

    case N_Integer_Literal:
      {
	tree gnu_type;

	/* Get the type of the result, looking inside any padding and
	   justified modular types.  Then get the value in that type.  */
	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_type) == RECORD_TYPE
	    && TYPE_JUSTIFIED_MODULAR_P (gnu_type))
	  gnu_type = TREE_TYPE (TYPE_FIELDS (gnu_type));

	gnu_result = UI_To_gnu (Intval (gnat_node), gnu_type);

	/* If the result overflows (meaning it doesn't fit in its base type),
	   abort.  We would like to check that the value is within the range
	   of the subtype, but that causes problems with subtypes whose usage
	   will raise Constraint_Error and with biased representation, so
	   we don't.  */
	gcc_assert (!TREE_CONSTANT_OVERFLOW (gnu_result));
      }
      break;

    case N_Character_Literal:
      /* If a Entity is present, it means that this was one of the
	 literals in a user-defined character type.  In that case,
	 just return the value in the CONST_DECL.  Otherwise, use the
	 character code.  In that case, the base type should be an
	 INTEGER_TYPE, but we won't bother checking for that.  */
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (Present (Entity (gnat_node)))
	gnu_result = DECL_INITIAL (get_gnu_tree (Entity (gnat_node)));
      else
	gnu_result
	  = force_fit_type
	    (build_int_cst
	      (gnu_result_type, UI_To_CC (Char_Literal_Value (gnat_node))),
	     false, false, false);
      break;

    case N_Real_Literal:
      /* If this is of a fixed-point type, the value we want is the
	 value of the corresponding integer.  */
      if (IN (Ekind (Underlying_Type (Etype (gnat_node))), Fixed_Point_Kind))
	{
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = UI_To_gnu (Corresponding_Integer_Value (gnat_node),
				  gnu_result_type);
	  gcc_assert (!TREE_CONSTANT_OVERFLOW (gnu_result));
	}

      /* We should never see a Vax_Float type literal, since the front end
         is supposed to transform these using appropriate conversions */
      else if (Vax_Float (Underlying_Type (Etype (gnat_node))))
	gcc_unreachable ();

      else
        {
	  Ureal ur_realval = Realval (gnat_node);

	  gnu_result_type = get_unpadded_type (Etype (gnat_node));

	  /* If the real value is zero, so is the result.  Otherwise,
	     convert it to a machine number if it isn't already.  That
	     forces BASE to 0 or 2 and simplifies the rest of our logic.  */
	  if (UR_Is_Zero (ur_realval))
	    gnu_result = convert (gnu_result_type, integer_zero_node);
	  else
	    {
	      if (!Is_Machine_Number (gnat_node))
		ur_realval
		  = Machine (Base_Type (Underlying_Type (Etype (gnat_node))),
			     ur_realval, Round_Even, gnat_node);

	      gnu_result
		= UI_To_gnu (Numerator (ur_realval), gnu_result_type);

	      /* If we have a base of zero, divide by the denominator.
		 Otherwise, the base must be 2 and we scale the value, which
		 we know can fit in the mantissa of the type (hence the use
		 of that type above).  */
	      if (No (Rbase (ur_realval)))
		gnu_result
		  = build_binary_op (RDIV_EXPR,
				     get_base_type (gnu_result_type),
				     gnu_result,
				     UI_To_gnu (Denominator (ur_realval),
						gnu_result_type));
	      else
		{
		  REAL_VALUE_TYPE tmp;

		  gcc_assert (Rbase (ur_realval) == 2);
		  real_ldexp (&tmp, &TREE_REAL_CST (gnu_result),
			      - UI_To_Int (Denominator (ur_realval)));
		  gnu_result = build_real (gnu_result_type, tmp);
		}
	    }

	  /* Now see if we need to negate the result.  Do it this way to
	     properly handle -0.  */
	  if (UR_Is_Negative (Realval (gnat_node)))
	    gnu_result
	      = build_unary_op (NEGATE_EXPR, get_base_type (gnu_result_type),
				gnu_result);
	}

      break;

    case N_String_Literal:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (TYPE_PRECISION (TREE_TYPE (gnu_result_type)) == HOST_BITS_PER_CHAR)
	{
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  char *string = (char *) alloca (length + 1);
	  int i;

	  /* Build the string with the characters in the literal.  Note
	     that Ada strings are 1-origin.  */
	  for (i = 0; i < length; i++)
	    string[i] = Get_String_Char (gnat_string, i + 1);

	  /* Put a null at the end of the string in case it's in a context
	     where GCC will want to treat it as a C string.  */
	  string[i] = 0;

	  gnu_result = build_string (length, string);

	  /* Strings in GCC don't normally have types, but we want
	     this to not be converted to the array type.  */
	  TREE_TYPE (gnu_result) = gnu_result_type;
	}
      else
	{
	  /* Build a list consisting of each character, then make
	     the aggregate.  */
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  int i;
	  tree gnu_list = NULL_TREE;
	  tree gnu_idx = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type));

	  for (i = 0; i < length; i++)
	    {
	      gnu_list
		= tree_cons (gnu_idx,
			     build_int_cst (TREE_TYPE (gnu_result_type),
					    Get_String_Char (gnat_string,
							     i + 1)),
			     gnu_list);

	      gnu_idx = int_const_binop (PLUS_EXPR, gnu_idx, integer_one_node,
					 0);
	    }

	  gnu_result
	    = gnat_build_constructor (gnu_result_type, nreverse (gnu_list));
	}
      break;

    case N_Pragma:
      gnu_result = Pragma_to_gnu (gnat_node);
      break;

    /**************************************/
    /* Chapter 3: Declarations and Types: */
    /**************************************/

    case N_Subtype_Declaration:
    case N_Full_Type_Declaration:
    case N_Incomplete_Type_Declaration:
    case N_Private_Type_Declaration:
    case N_Private_Extension_Declaration:
    case N_Task_Type_Declaration:
      process_type (Defining_Entity (gnat_node));
      gnu_result = alloc_stmt_list ();
      break;

    case N_Object_Declaration:
    case N_Exception_Declaration:
      gnat_temp = Defining_Entity (gnat_node);
      gnu_result = alloc_stmt_list ();

      /* If we are just annotating types and this object has an unconstrained
	 or task type, don't elaborate it.   */
      if (type_annotate_only
	  && (((Is_Array_Type (Etype (gnat_temp))
		|| Is_Record_Type (Etype (gnat_temp)))
	       && !Is_Constrained (Etype (gnat_temp)))
	    || Is_Concurrent_Type (Etype (gnat_temp))))
	break;

      if (Present (Expression (gnat_node))
	  && !(Nkind (gnat_node) == N_Object_Declaration
	       && No_Initialization (gnat_node))
	  && (!type_annotate_only
	      || Compile_Time_Known_Value (Expression (gnat_node))))
	{
	  gnu_expr = gnat_to_gnu (Expression (gnat_node));
	  if (Do_Range_Check (Expression (gnat_node)))
	    gnu_expr = emit_range_check (gnu_expr, Etype (gnat_temp));

	  /* If this object has its elaboration delayed, we must force
	     evaluation of GNU_EXPR right now and save it for when the object
	     is frozen.  */
	  if (Present (Freeze_Node (gnat_temp)))
	    {
	      if ((Is_Public (gnat_temp) || global_bindings_p ())
		  && !TREE_CONSTANT (gnu_expr))
		gnu_expr
		  = create_var_decl (create_concat_name (gnat_temp, "init"),
				     NULL_TREE, TREE_TYPE (gnu_expr),
				     gnu_expr, false, Is_Public (gnat_temp),
				     false, false, NULL, gnat_temp);
	      else
		gnu_expr = maybe_variable (gnu_expr);

	      save_gnu_tree (gnat_node, gnu_expr, true);
	    }
	}
      else
	gnu_expr = NULL_TREE;

      if (type_annotate_only && gnu_expr && TREE_CODE (gnu_expr) == ERROR_MARK)
	gnu_expr = NULL_TREE;

      if (No (Freeze_Node (gnat_temp)))
	gnat_to_gnu_entity (gnat_temp, gnu_expr, 1);
      break;

    case N_Object_Renaming_Declaration:
      gnat_temp = Defining_Entity (gnat_node);

      /* Don't do anything if this renaming is handled by the front end or if
	 we are just annotating types and this object has a composite or task
	 type, don't elaborate it.  We return the result in case it has any
	 SAVE_EXPRs in it that need to be evaluated here.  */
      if (!Is_Renaming_Of_Object (gnat_temp)
	  && ! (type_annotate_only
		&& (Is_Array_Type (Etype (gnat_temp))
		    || Is_Record_Type (Etype (gnat_temp))
		    || Is_Concurrent_Type (Etype (gnat_temp)))))
	gnu_result
	  = gnat_to_gnu_entity (gnat_temp,
				gnat_to_gnu (Renamed_Object (gnat_temp)), 1);
      else
	gnu_result = alloc_stmt_list ();
      break;

    case N_Implicit_Label_Declaration:
      gnat_to_gnu_entity (Defining_Entity (gnat_node), NULL_TREE, 1);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Exception_Renaming_Declaration:
    case N_Number_Declaration:
    case N_Package_Renaming_Declaration:
    case N_Subprogram_Renaming_Declaration:
      /* These are fully handled in the front end.  */
      gnu_result = alloc_stmt_list ();
      break;

    /*************************************/
    /* Chapter 4: Names and Expressions: */
    /*************************************/

    case N_Explicit_Dereference:
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);
      break;

    case N_Indexed_Component:
      {
	tree gnu_array_object = gnat_to_gnu (Prefix (gnat_node));
	tree gnu_type;
	int ndim;
	int i;
	Node_Id *gnat_expr_array;

	gnu_array_object = maybe_implicit_deref (gnu_array_object);
	gnu_array_object = maybe_unconstrained_array (gnu_array_object);

	/* If we got a padded type, remove it too.  */
	if (TREE_CODE (TREE_TYPE (gnu_array_object)) == RECORD_TYPE
	    && TYPE_IS_PADDING_P (TREE_TYPE (gnu_array_object)))
	  gnu_array_object
	    = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_array_object))),
		       gnu_array_object);

	gnu_result = gnu_array_object;

	/* First compute the number of dimensions of the array, then
	   fill the expression array, the order depending on whether
	   this is a Convention_Fortran array or not.  */
	for (ndim = 1, gnu_type = TREE_TYPE (gnu_array_object);
	     TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type));
	     ndim++, gnu_type = TREE_TYPE (gnu_type))
	  ;

	gnat_expr_array = (Node_Id *) alloca (ndim * sizeof (Node_Id));

	if (TYPE_CONVENTION_FORTRAN_P (TREE_TYPE (gnu_array_object)))
	  for (i = ndim - 1, gnat_temp = First (Expressions (gnat_node));
	       i >= 0;
	       i--, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;
	else
	  for (i = 0, gnat_temp = First (Expressions (gnat_node));
	       i < ndim;
	       i++, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;

	for (i = 0, gnu_type = TREE_TYPE (gnu_array_object);
	     i < ndim; i++, gnu_type = TREE_TYPE (gnu_type))
	  {
	    gcc_assert (TREE_CODE (gnu_type) == ARRAY_TYPE);
	    gnat_temp = gnat_expr_array[i];
	    gnu_expr = gnat_to_gnu (gnat_temp);

	    if (Do_Range_Check (gnat_temp))
	      gnu_expr
		= emit_index_check
		  (gnu_array_object, gnu_expr,
		   TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		   TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

	    gnu_result = build_binary_op (ARRAY_REF, NULL_TREE,
					  gnu_result, gnu_expr);
	  }
      }

      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Slice:
      {
        tree gnu_type;
        Node_Id gnat_range_node = Discrete_Range (gnat_node);

        gnu_result = gnat_to_gnu (Prefix (gnat_node));
        gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* Do any implicit dereferences of the prefix and do any needed
	   range check.  */
        gnu_result = maybe_implicit_deref (gnu_result);
        gnu_result = maybe_unconstrained_array (gnu_result);
        gnu_type = TREE_TYPE (gnu_result);
        if (Do_Range_Check (gnat_range_node))
          {
            /* Get the bounds of the slice. */
	    tree gnu_index_type
	      = TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_result_type));
            tree gnu_min_expr = TYPE_MIN_VALUE (gnu_index_type);
            tree gnu_max_expr = TYPE_MAX_VALUE (gnu_index_type);
            tree gnu_expr_l, gnu_expr_h, gnu_expr_type;

            /* Check to see that the minimum slice value is in range */
            gnu_expr_l
	      = emit_index_check
		(gnu_result, gnu_min_expr,
		 TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		 TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

            /* Check to see that the maximum slice value is in range */
            gnu_expr_h
	      = emit_index_check
		(gnu_result, gnu_max_expr,
		 TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		 TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

            /* Derive a good type to convert everything too */
            gnu_expr_type = get_base_type (TREE_TYPE (gnu_expr_l));

            /* Build a compound expression that does the range checks */
            gnu_expr
              = build_binary_op (COMPOUND_EXPR, gnu_expr_type,
                                 convert (gnu_expr_type, gnu_expr_h),
                                 convert (gnu_expr_type, gnu_expr_l));

            /* Build a conditional expression that returns the range checks
               expression if the slice range is not null (max >= min) or
               returns the min if the slice range is null */
            gnu_expr
              = fold (build3 (COND_EXPR, gnu_expr_type,
			      build_binary_op (GE_EXPR, gnu_expr_type,
					       convert (gnu_expr_type,
							gnu_max_expr),
					       convert (gnu_expr_type,
							gnu_min_expr)),
			      gnu_expr, gnu_min_expr));
          }
        else
          gnu_expr = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type));

        gnu_result = build_binary_op (ARRAY_RANGE_REF, gnu_result_type,
				      gnu_result, gnu_expr);
      }
      break;

    case N_Selected_Component:
      {
	tree gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
	Entity_Id gnat_field = Entity (Selector_Name (gnat_node));
	Entity_Id gnat_pref_type = Etype (Prefix (gnat_node));
	tree gnu_field;

	while (IN (Ekind (gnat_pref_type), Incomplete_Or_Private_Kind)
	       || IN (Ekind (gnat_pref_type), Access_Kind))
	  {
	    if (IN (Ekind (gnat_pref_type), Incomplete_Or_Private_Kind))
	      gnat_pref_type = Underlying_Type (gnat_pref_type);
	    else if (IN (Ekind (gnat_pref_type), Access_Kind))
	      gnat_pref_type = Designated_Type (gnat_pref_type);
	  }

	gnu_prefix = maybe_implicit_deref (gnu_prefix);

	/* For discriminant references in tagged types always substitute the
	   corresponding discriminant as the actual selected component. */

	if (Is_Tagged_Type (gnat_pref_type))
	  while (Present (Corresponding_Discriminant (gnat_field)))
	    gnat_field = Corresponding_Discriminant (gnat_field);

	/* For discriminant references of untagged types always substitute the
	   corresponding stored discriminant. */

	else if (Present (Corresponding_Discriminant (gnat_field)))
	  gnat_field = Original_Record_Component (gnat_field);

	/* Handle extracting the real or imaginary part of a complex.
	   The real part is the first field and the imaginary the last.  */

	if (TREE_CODE (TREE_TYPE (gnu_prefix)) == COMPLEX_TYPE)
	  gnu_result = build_unary_op (Present (Next_Entity (gnat_field))
				       ? REALPART_EXPR : IMAGPART_EXPR,
				       NULL_TREE, gnu_prefix);
	else
	  {
	    gnu_field = gnat_to_gnu_field_decl (gnat_field);

	    /* If there are discriminants, the prefix might be
               evaluated more than once, which is a problem if it has
               side-effects. */
	    if (Has_Discriminants (Is_Access_Type (Etype (Prefix (gnat_node)))
				   ? Designated_Type (Etype
						      (Prefix (gnat_node)))
				   : Etype (Prefix (gnat_node))))
	      gnu_prefix = gnat_stabilize_reference (gnu_prefix, false);

	    gnu_result
	      = build_component_ref (gnu_prefix, NULL_TREE, gnu_field,
				     (Nkind (Parent (gnat_node))
				      == N_Attribute_Reference));
	  }

	gcc_assert (gnu_result);
	gnu_result_type = get_unpadded_type (Etype (gnat_node));
      }
      break;

    case N_Attribute_Reference:
      {
        /* The attribute designator (like an enumeration value). */
        int attribute = Get_Attribute_Id (Attribute_Name (gnat_node));

	/* The Elab_Spec and Elab_Body attributes are special in that
	   Prefix is a unit, not an object with a GCC equivalent.  Similarly
	   for Elaborated, since that variable isn't otherwise known.  */
	if (attribute == Attr_Elab_Body || attribute == Attr_Elab_Spec)
	  return (create_subprog_decl
		  (create_concat_name (Entity (Prefix (gnat_node)),
				       attribute == Attr_Elab_Body
				       ? "elabb" : "elabs"),
		   NULL_TREE, void_ftype, NULL_TREE, false, true, true, NULL,
		   gnat_node));

	gnu_result = Attribute_to_gnu (gnat_node, &gnu_result_type, attribute);
      }
      break;

    case N_Reference:
      /* Like 'Access as far as we are concerned.  */
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_result);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Aggregate:
    case N_Extension_Aggregate:
      {
	tree gnu_aggr_type;

	/* ??? It is wrong to evaluate the type now, but there doesn't
	   seem to be any other practical way of doing it.  */

	gcc_assert (!Expansion_Delayed (gnat_node));

	gnu_aggr_type = gnu_result_type
	  = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_result_type) == RECORD_TYPE
	    && TYPE_CONTAINS_TEMPLATE_P (gnu_result_type))
	  gnu_aggr_type
	    = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_result_type)));

	if (Null_Record_Present (gnat_node))
	  gnu_result = gnat_build_constructor (gnu_aggr_type, NULL_TREE);

	else if (TREE_CODE (gnu_aggr_type) == UNION_TYPE
		 && TYPE_UNCHECKED_UNION_P (gnu_aggr_type))
	  {
	    /* The first element is the discrimant, which we ignore.  The
	       next is the field we're building.  Convert the expression
	       to the type of the field and then to the union type.  */
	    Node_Id gnat_assoc
	      = Next (First (Component_Associations (gnat_node)));
	    Entity_Id gnat_field = Entity (First (Choices (gnat_assoc)));
	    tree gnu_field_type
	      = TREE_TYPE (gnat_to_gnu_entity (gnat_field, NULL_TREE, 0));

	    gnu_result = convert (gnu_field_type,
				  gnat_to_gnu (Expression (gnat_assoc)));
	  }
	else if (TREE_CODE (gnu_aggr_type) == RECORD_TYPE
		 || TREE_CODE (gnu_aggr_type) == UNION_TYPE)
	  gnu_result
	    = assoc_to_constructor (First (Component_Associations (gnat_node)),
				    gnu_aggr_type);
	else if (TREE_CODE (gnu_aggr_type) == ARRAY_TYPE)
	  gnu_result = pos_to_constructor (First (Expressions (gnat_node)),
					   gnu_aggr_type,
					   Component_Type (Etype (gnat_node)));
	else if (TREE_CODE (gnu_aggr_type) == COMPLEX_TYPE)
	  gnu_result
	    = build_binary_op
	      (COMPLEX_EXPR, gnu_aggr_type,
	       gnat_to_gnu (Expression (First
					(Component_Associations (gnat_node)))),
	       gnat_to_gnu (Expression
			    (Next
			     (First (Component_Associations (gnat_node))))));
	else
	  gcc_unreachable ();

	gnu_result = convert (gnu_result_type, gnu_result);
      }
      break;

    case N_Null:
      gnu_result = null_pointer_node;
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Type_Conversion:
    case N_Qualified_Expression:
      /* Get the operand expression.  */
      gnu_result = gnat_to_gnu (Expression (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      gnu_result
	= convert_with_check (Etype (gnat_node), gnu_result,
			      Do_Overflow_Check (gnat_node),
			      Do_Range_Check (Expression (gnat_node)),
			      Nkind (gnat_node) == N_Type_Conversion
			      && Float_Truncate (gnat_node));
      break;

    case N_Unchecked_Type_Conversion:
      gnu_result = gnat_to_gnu (Expression (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* If the result is a pointer type, see if we are improperly
	 converting to a stricter alignment.  */

      if (STRICT_ALIGNMENT && POINTER_TYPE_P (gnu_result_type)
	  && IN (Ekind (Etype (gnat_node)), Access_Kind))
	{
	  unsigned int align = known_alignment (gnu_result);
	  tree gnu_obj_type = TREE_TYPE (gnu_result_type);
	  unsigned int oalign = TYPE_ALIGN (gnu_obj_type);

	  if (align != 0 && align < oalign && !TYPE_ALIGN_OK (gnu_obj_type))
	    post_error_ne_tree_2
	      ("?source alignment (^) '< alignment of & (^)",
	       gnat_node, Designated_Type (Etype (gnat_node)),
	       size_int (align / BITS_PER_UNIT), oalign / BITS_PER_UNIT);
	}

      gnu_result = unchecked_convert (gnu_result_type, gnu_result,
				      No_Truncation (gnat_node));
      break;

    case N_In:
    case N_Not_In:
      {
	tree gnu_object = gnat_to_gnu (Left_Opnd (gnat_node));
	Node_Id gnat_range = Right_Opnd (gnat_node);
	tree gnu_low;
	tree gnu_high;

	/* GNAT_RANGE is either an N_Range node or an identifier
	   denoting a subtype.  */
	if (Nkind (gnat_range) == N_Range)
	  {
	    gnu_low = gnat_to_gnu (Low_Bound (gnat_range));
	    gnu_high = gnat_to_gnu (High_Bound (gnat_range));
	  }
	else if (Nkind (gnat_range) == N_Identifier
              || Nkind (gnat_range) == N_Expanded_Name)
	  {
	    tree gnu_range_type = get_unpadded_type (Entity (gnat_range));

	    gnu_low = TYPE_MIN_VALUE (gnu_range_type);
	    gnu_high = TYPE_MAX_VALUE (gnu_range_type);
	  }
	else
	  gcc_unreachable ();

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If LOW and HIGH are identical, perform an equality test.
	   Otherwise, ensure that GNU_OBJECT is only evaluated once
	   and perform a full range test.  */
	if (operand_equal_p (gnu_low, gnu_high, 0))
	  gnu_result = build_binary_op (EQ_EXPR, gnu_result_type,
					gnu_object, gnu_low);
	else
	  {
	    gnu_object = protect_multiple_eval (gnu_object);
	    gnu_result
	      = build_binary_op (TRUTH_ANDIF_EXPR, gnu_result_type,
				 build_binary_op (GE_EXPR, gnu_result_type,
						  gnu_object, gnu_low),
				 build_binary_op (LE_EXPR, gnu_result_type,
						  gnu_object, gnu_high));
	  }

	if (Nkind (gnat_node) == N_Not_In)
	  gnu_result = invert_truthvalue (gnu_result);
      }
      break;

    case N_Op_Divide:
      gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
      gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_binary_op (FLOAT_TYPE_P (gnu_result_type)
				    ? RDIV_EXPR
				    : (Rounded_Result (gnat_node)
				       ? ROUND_DIV_EXPR : TRUNC_DIV_EXPR),
				    gnu_result_type, gnu_lhs, gnu_rhs);
      break;

    case N_Op_Or:    case N_Op_And:      case N_Op_Xor:
      /* These can either be operations on booleans or on modular types.
	 Fall through for boolean types since that's the way GNU_CODES is
	 set up.  */
      if (IN (Ekind (Underlying_Type (Etype (gnat_node))),
	      Modular_Integer_Kind))
	{
	  enum tree_code code
	    = (Nkind (gnat_node) == N_Op_Or ? BIT_IOR_EXPR
	       : Nkind (gnat_node) == N_Op_And ? BIT_AND_EXPR
	       : BIT_XOR_EXPR);

	  gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	  gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_binary_op (code, gnu_result_type,
					gnu_lhs, gnu_rhs);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Eq:    case N_Op_Ne:	 case N_Op_Lt:
    case N_Op_Le:    case N_Op_Gt:       case N_Op_Ge:
    case N_Op_Add:   case N_Op_Subtract: case N_Op_Multiply:
    case N_Op_Mod:   case N_Op_Rem:
    case N_Op_Rotate_Left:
    case N_Op_Rotate_Right:
    case N_Op_Shift_Left:
    case N_Op_Shift_Right:
    case N_Op_Shift_Right_Arithmetic:
    case N_And_Then: case N_Or_Else:
      {
	enum tree_code code = gnu_codes[Nkind (gnat_node)];
	tree gnu_type;

	gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If this is a comparison operator, convert any references to
	   an unconstrained array value into a reference to the
	   actual array.  */
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    gnu_lhs = maybe_unconstrained_array (gnu_lhs);
	    gnu_rhs = maybe_unconstrained_array (gnu_rhs);
	  }

	/* If the result type is a private type, its full view may be a
	   numeric subtype. The representation we need is that of its base
	   type, given that it is the result of an arithmetic operation.  */
        else if (Is_Private_Type (Etype (gnat_node)))
	  gnu_type = gnu_result_type
	    = get_unpadded_type (Base_Type (Full_View (Etype (gnat_node))));

	/* If this is a shift whose count is not guaranteed to be correct,
	   we need to adjust the shift count.  */
	if (IN (Nkind (gnat_node), N_Op_Shift)
	    && !Shift_Count_OK (gnat_node))
	  {
	    tree gnu_count_type = get_base_type (TREE_TYPE (gnu_rhs));
	    tree gnu_max_shift
	      = convert (gnu_count_type, TYPE_SIZE (gnu_type));

	    if (Nkind (gnat_node) == N_Op_Rotate_Left
		|| Nkind (gnat_node) == N_Op_Rotate_Right)
	      gnu_rhs = build_binary_op (TRUNC_MOD_EXPR, gnu_count_type,
					 gnu_rhs, gnu_max_shift);
	    else if (Nkind (gnat_node) == N_Op_Shift_Right_Arithmetic)
	      gnu_rhs
		= build_binary_op
		  (MIN_EXPR, gnu_count_type,
		   build_binary_op (MINUS_EXPR,
				    gnu_count_type,
				    gnu_max_shift,
				    convert (gnu_count_type,
					     integer_one_node)),
		   gnu_rhs);
	  }

	/* For right shifts, the type says what kind of shift to do,
	   so we may need to choose a different type.  */
	if (Nkind (gnat_node) == N_Op_Shift_Right
	    && !TYPE_UNSIGNED (gnu_type))
	  gnu_type = gnat_unsigned_type (gnu_type);
	else if (Nkind (gnat_node) == N_Op_Shift_Right_Arithmetic
		 && TYPE_UNSIGNED (gnu_type))
	  gnu_type = gnat_signed_type (gnu_type);

	if (gnu_type != gnu_result_type)
	  {
	    gnu_lhs = convert (gnu_type, gnu_lhs);
	    gnu_rhs = convert (gnu_type, gnu_rhs);
	  }

	gnu_result = build_binary_op (code, gnu_type, gnu_lhs, gnu_rhs);

	/* If this is a logical shift with the shift count not verified,
	   we must return zero if it is too large.  We cannot compensate
	   above in this case.  */
	if ((Nkind (gnat_node) == N_Op_Shift_Left
	     || Nkind (gnat_node) == N_Op_Shift_Right)
	    && !Shift_Count_OK (gnat_node))
	  gnu_result
	    = build_cond_expr
	      (gnu_type,
	       build_binary_op (GE_EXPR, integer_type_node,
				gnu_rhs,
				convert (TREE_TYPE (gnu_rhs),
					 TYPE_SIZE (gnu_type))),
	       convert (gnu_type, integer_zero_node),
	       gnu_result);
      }
      break;

    case N_Conditional_Expression:
      {
        tree gnu_cond = gnat_to_gnu (First (Expressions (gnat_node)));
        tree gnu_true = gnat_to_gnu (Next (First (Expressions (gnat_node))));
        tree gnu_false
          = gnat_to_gnu (Next (Next (First (Expressions (gnat_node)))));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_result = build_cond_expr (gnu_result_type,
				      gnat_truthvalue_conversion (gnu_cond),
				      gnu_true, gnu_false);
      }
      break;

    case N_Op_Plus:
      gnu_result = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Op_Not:
      /* This case can apply to a boolean or a modular type.
	 Fall through for a boolean operand since GNU_CODES is set
	 up to handle this.  */
      if (IN (Ekind (Etype (gnat_node)), Modular_Integer_Kind))
	{
	  gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_unary_op (BIT_NOT_EXPR, gnu_result_type,
				       gnu_expr);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Minus:  case N_Op_Abs:
      gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));

      if (Ekind (Etype (gnat_node)) != E_Private_Type)
         gnu_result_type = get_unpadded_type (Etype (gnat_node));
      else
         gnu_result_type = get_unpadded_type (Base_Type
					      (Full_View (Etype (gnat_node))));

      gnu_result = build_unary_op (gnu_codes[Nkind (gnat_node)],
				   gnu_result_type, gnu_expr);
      break;

    case N_Allocator:
      {
	tree gnu_init = 0;
	tree gnu_type;
	bool ignore_init_type = false;

	gnat_temp = Expression (gnat_node);

	/* The Expression operand can either be an N_Identifier or
	   Expanded_Name, which must represent a type, or a
	   N_Qualified_Expression, which contains both the object type and an
	   initial value for the object.  */
	if (Nkind (gnat_temp) == N_Identifier
	    || Nkind (gnat_temp) == N_Expanded_Name)
	  gnu_type = gnat_to_gnu_type (Entity (gnat_temp));
	else if (Nkind (gnat_temp) == N_Qualified_Expression)
	  {
	    Entity_Id gnat_desig_type
	      = Designated_Type (Underlying_Type (Etype (gnat_node)));

	    ignore_init_type = Has_Constrained_Partial_View (gnat_desig_type);
	    gnu_init = gnat_to_gnu (Expression (gnat_temp));

	    gnu_init = maybe_unconstrained_array (gnu_init);
            if (Do_Range_Check (Expression (gnat_temp)))
              gnu_init = emit_range_check (gnu_init, gnat_desig_type);

	    if (Is_Elementary_Type (gnat_desig_type)
		|| Is_Constrained (gnat_desig_type))
	      {
		gnu_type = gnat_to_gnu_type (gnat_desig_type);
		gnu_init = convert (gnu_type, gnu_init);
	      }
	    else
	      {
		gnu_type = gnat_to_gnu_type (Etype (Expression (gnat_temp)));
		if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
		  gnu_type = TREE_TYPE (gnu_init);

		gnu_init = convert (gnu_type, gnu_init);
	      }
	  }
	else
	  gcc_unreachable ();

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	return build_allocator (gnu_type, gnu_init, gnu_result_type,
				Procedure_To_Call (gnat_node),
				Storage_Pool (gnat_node), gnat_node,
				ignore_init_type);
      }
      break;

    /***************************/
    /* Chapter 5: Statements:  */
    /***************************/

    case N_Label:
      gnu_result = build1 (LABEL_EXPR, void_type_node,
			   gnat_to_gnu (Identifier (gnat_node)));
      break;

    case N_Null_Statement:
      gnu_result = alloc_stmt_list ();
      break;

    case N_Assignment_Statement:
      /* Get the LHS and RHS of the statement and convert any reference to an
	 unconstrained array into a reference to the underlying array.
	 If we are not to do range checking and the RHS is an N_Function_Call,
	 pass the LHS to the call function.  */
      gnu_lhs = maybe_unconstrained_array (gnat_to_gnu (Name (gnat_node)));

      /* If the type has a size that overflows, convert this into raise of
	 Storage_Error: execution shouldn't have gotten here anyway.  */
      if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (gnu_lhs))) == INTEGER_CST
	   && TREE_OVERFLOW (TYPE_SIZE_UNIT (TREE_TYPE (gnu_lhs))))
	gnu_result = build_call_raise (SE_Object_Too_Large, gnat_node);
      else if (Nkind (Expression (gnat_node)) == N_Function_Call
	       && !Do_Range_Check (Expression (gnat_node)))
	gnu_result = call_to_gnu (Expression (gnat_node),
				  &gnu_result_type, gnu_lhs);
      else
	{
	  gnu_rhs
	    = maybe_unconstrained_array (gnat_to_gnu (Expression (gnat_node)));

	  /* If range check is needed, emit code to generate it */
	  if (Do_Range_Check (Expression (gnat_node)))
	    gnu_rhs = emit_range_check (gnu_rhs, Etype (Name (gnat_node)));

	  gnu_result
	    = build_binary_op (MODIFY_EXPR, NULL_TREE, gnu_lhs, gnu_rhs);
	}
      break;

    case N_If_Statement:
      {
	tree *gnu_else_ptr;	/* Point to put next "else if" or "else". */

	/* Make the outer COND_EXPR.  Avoid non-determinism.  */
	gnu_result = build3 (COND_EXPR, void_type_node,
			     gnat_to_gnu (Condition (gnat_node)),
			     NULL_TREE, NULL_TREE);
	COND_EXPR_THEN (gnu_result)
	  = build_stmt_group (Then_Statements (gnat_node), false);
	TREE_SIDE_EFFECTS (gnu_result) = 1;
	gnu_else_ptr = &COND_EXPR_ELSE (gnu_result);

	/* Now make a COND_EXPR for each of the "else if" parts.  Put each
	   into the previous "else" part and point to where to put any
	   outer "else".  Also avoid non-determinism.  */
	if (Present (Elsif_Parts (gnat_node)))
	  for (gnat_temp = First (Elsif_Parts (gnat_node));
	       Present (gnat_temp); gnat_temp = Next (gnat_temp))
	    {
	      gnu_expr = build3 (COND_EXPR, void_type_node,
				 gnat_to_gnu (Condition (gnat_temp)),
				 NULL_TREE, NULL_TREE);
	      COND_EXPR_THEN (gnu_expr)
		= build_stmt_group (Then_Statements (gnat_temp), false);
	      TREE_SIDE_EFFECTS (gnu_expr) = 1;
	      annotate_with_node (gnu_expr, gnat_temp);
	      *gnu_else_ptr = gnu_expr;
	      gnu_else_ptr = &COND_EXPR_ELSE (gnu_expr);
	    }

	*gnu_else_ptr = build_stmt_group (Else_Statements (gnat_node), false);
      }
      break;

    case N_Case_Statement:
      gnu_result = Case_Statement_to_gnu (gnat_node);
      break;

    case N_Loop_Statement:
      gnu_result = Loop_Statement_to_gnu (gnat_node);
      break;

    case N_Block_Statement:
      start_stmt_group ();
      gnat_pushlevel ();
      process_decls (Declarations (gnat_node), Empty, Empty, true, true);
      add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));
      gnat_poplevel ();
      gnu_result = end_stmt_group ();

      if (Present (Identifier (gnat_node)))
	mark_out_of_scope (Entity (Identifier (gnat_node)));
      break;

    case N_Exit_Statement:
      gnu_result
	= build2 (EXIT_STMT, void_type_node,
		  (Present (Condition (gnat_node))
		   ? gnat_to_gnu (Condition (gnat_node)) : NULL_TREE),
		  (Present (Name (gnat_node))
		   ? get_gnu_tree (Entity (Name (gnat_node)))
		   : TREE_VALUE (gnu_loop_label_stack)));
      break;

    case N_Return_Statement:
      {
	/* The gnu function type of the subprogram currently processed.  */
	tree gnu_subprog_type = TREE_TYPE (current_function_decl);
	/* The return value from the subprogram.  */
	tree gnu_ret_val = NULL_TREE;
	/* The place to put the return value.  */
	tree gnu_lhs;

	/* If we are dealing with a "return;" from an Ada procedure with
	   parameters passed by copy in copy out, we need to return a record
	   containing the final values of these parameters.  If the list
	   contains only one entry, return just that entry.

	   For a full description of the copy in copy out parameter mechanism,
	   see the part of the gnat_to_gnu_entity routine dealing with the
	   translation of subprograms.

	   But if we have a return label defined, convert this into
	   a branch to that label.  */

	if (TREE_VALUE (gnu_return_label_stack))
	  {
	    gnu_result = build1 (GOTO_EXPR, void_type_node,
				 TREE_VALUE (gnu_return_label_stack));
	    break;
	  }

	else if (TYPE_CI_CO_LIST (gnu_subprog_type))
	  {
	    gnu_lhs = DECL_RESULT (current_function_decl);
	    if (list_length (TYPE_CI_CO_LIST (gnu_subprog_type)) == 1)
	      gnu_ret_val = TREE_VALUE (TYPE_CI_CO_LIST (gnu_subprog_type));
	    else
	      gnu_ret_val
		= gnat_build_constructor (TREE_TYPE (gnu_subprog_type),
					  TYPE_CI_CO_LIST (gnu_subprog_type));
	  }

	/* If the Ada subprogram is a function, we just need to return the
	   expression.   If the subprogram returns an unconstrained
	   array, we have to allocate a new version of the result and
	   return it.  If we return by reference, return a pointer.  */

	else if (Present (Expression (gnat_node)))
	  {
	    /* If the current function returns by target pointer and we
	       are doing a call, pass that target to the call.  */
	    if (TYPE_RETURNS_BY_TARGET_PTR_P (gnu_subprog_type)
		&& Nkind (Expression (gnat_node)) == N_Function_Call)
	      {
	        gnu_lhs
		  = build_unary_op (INDIRECT_REF, NULL_TREE,
				    DECL_ARGUMENTS (current_function_decl));
		gnu_result = call_to_gnu (Expression (gnat_node),
					  &gnu_result_type, gnu_lhs);
	      }
	    else
	      {
		gnu_ret_val = gnat_to_gnu (Expression (gnat_node));

		if (TYPE_RETURNS_BY_TARGET_PTR_P (gnu_subprog_type))
		  /* The original return type was unconstrained so dereference
		     the TARGET pointer in the actual return value's type. */
		  gnu_lhs
		    = build_unary_op (INDIRECT_REF, TREE_TYPE (gnu_ret_val),
				      DECL_ARGUMENTS (current_function_decl));
		else
		  gnu_lhs = DECL_RESULT (current_function_decl);

		/* Do not remove the padding from GNU_RET_VAL if the inner
		   type is self-referential since we want to allocate the fixed
		   size in that case.  */
		if (TREE_CODE (gnu_ret_val) == COMPONENT_REF
		    && (TREE_CODE (TREE_TYPE (TREE_OPERAND (gnu_ret_val, 0)))
			== RECORD_TYPE)
		    && (TYPE_IS_PADDING_P
			(TREE_TYPE (TREE_OPERAND (gnu_ret_val, 0))))
		    && (CONTAINS_PLACEHOLDER_P
			(TYPE_SIZE (TREE_TYPE (gnu_ret_val)))))
		  gnu_ret_val = TREE_OPERAND (gnu_ret_val, 0);

		if (TYPE_RETURNS_BY_REF_P (gnu_subprog_type)
		    || By_Ref (gnat_node))
		  gnu_ret_val
		    = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_ret_val);

		else if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type))
		  {
		    gnu_ret_val = maybe_unconstrained_array (gnu_ret_val);

		    /* We have two cases: either the function returns with
		       depressed stack or not.  If not, we allocate on the
		       secondary stack.  If so, we allocate in the stack frame.
		       if no copy is needed, the front end will set By_Ref,
		       which we handle in the case above.  */
		    if (TYPE_RETURNS_STACK_DEPRESSED (gnu_subprog_type))
		      gnu_ret_val
			= build_allocator (TREE_TYPE (gnu_ret_val),
					   gnu_ret_val,
					   TREE_TYPE (gnu_subprog_type),
					   0, -1, gnat_node, false);
		    else
		      gnu_ret_val
			= build_allocator (TREE_TYPE (gnu_ret_val),
					   gnu_ret_val,
					   TREE_TYPE (gnu_subprog_type),
					   Procedure_To_Call (gnat_node),
					   Storage_Pool (gnat_node),
					   gnat_node, false);
		  }
	      }
	  }
	else
	  /* If the Ada subprogram is a regular procedure, just return.  */
	  gnu_lhs = NULL_TREE;

	if (TYPE_RETURNS_BY_TARGET_PTR_P (gnu_subprog_type))
	  {
	    if (gnu_ret_val)
	      gnu_result = build_binary_op (MODIFY_EXPR, NULL_TREE,
					    gnu_lhs, gnu_ret_val);
	    add_stmt_with_node (gnu_result, gnat_node);
	    gnu_lhs = NULL_TREE;
	  }

	gnu_result = build_return_expr (gnu_lhs, gnu_ret_val);
      }
      break;

    case N_Goto_Statement:
      gnu_result = build1 (GOTO_EXPR, void_type_node,
			   gnat_to_gnu (Name (gnat_node)));
      break;

    /****************************/
    /* Chapter 6: Subprograms:  */
    /****************************/

    case N_Subprogram_Declaration:
      /* Unless there is a freeze node, declare the subprogram.  We consider
	 this a "definition" even though we're not generating code for
	 the subprogram because we will be making the corresponding GCC
	 node here.  */

      if (No (Freeze_Node (Defining_Entity (Specification (gnat_node)))))
	gnat_to_gnu_entity (Defining_Entity (Specification (gnat_node)),
			    NULL_TREE, 1);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Abstract_Subprogram_Declaration:
      /* This subprogram doesn't exist for code generation purposes, but we
	 have to elaborate the types of any parameters, unless they are
	 imported types (nothing to generate in this case).  */
      for (gnat_temp
	   = First_Formal (Defining_Entity (Specification (gnat_node)));
	   Present (gnat_temp);
	   gnat_temp = Next_Formal_With_Extras (gnat_temp))
	if (Is_Itype (Etype (gnat_temp))
	    && !From_With_Type (Etype (gnat_temp)))
	  gnat_to_gnu_entity (Etype (gnat_temp), NULL_TREE, 0);

      gnu_result = alloc_stmt_list ();
      break;

    case N_Defining_Program_Unit_Name:
      /* For a child unit identifier go up a level to get the
         specification.  We get this when we try to find the spec of
	 a child unit package that is the compilation unit being compiled. */
      gnu_result = gnat_to_gnu (Parent (gnat_node));
      break;

    case N_Subprogram_Body:
      Subprogram_Body_to_gnu (gnat_node);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Function_Call:
    case N_Procedure_Call_Statement:
      gnu_result = call_to_gnu (gnat_node, &gnu_result_type, NULL_TREE);
      break;

    /*************************/
    /* Chapter 7: Packages:  */
    /*************************/

    case N_Package_Declaration:
      gnu_result = gnat_to_gnu (Specification (gnat_node));
      break;

    case N_Package_Specification:

      start_stmt_group ();
      process_decls (Visible_Declarations (gnat_node),
		     Private_Declarations (gnat_node), Empty, true, true);
      gnu_result = end_stmt_group ();
      break;

    case N_Package_Body:

      /* If this is the body of a generic package - do nothing */
      if (Ekind (Corresponding_Spec (gnat_node)) == E_Generic_Package)
	{
	  gnu_result = alloc_stmt_list ();
	  break;
	}

      start_stmt_group ();
      process_decls (Declarations (gnat_node), Empty, Empty, true, true);

      if (Present (Handled_Statement_Sequence (gnat_node)))
	add_stmt (gnat_to_gnu (Handled_Statement_Sequence (gnat_node)));

      gnu_result = end_stmt_group ();
      break;

    /*********************************/
    /* Chapter 8: Visibility Rules:  */
    /*********************************/

    case N_Use_Package_Clause:
    case N_Use_Type_Clause:
      /* Nothing to do here - but these may appear in list of declarations */
      gnu_result = alloc_stmt_list ();
      break;

    /***********************/
    /* Chapter 9: Tasks:   */
    /***********************/

    case N_Protected_Type_Declaration:
      gnu_result = alloc_stmt_list ();
      break;

    case N_Single_Task_Declaration:
      gnat_to_gnu_entity (Defining_Entity (gnat_node), NULL_TREE, 1);
      gnu_result = alloc_stmt_list ();
      break;

    /***********************************************************/
    /* Chapter 10: Program Structure and Compilation Issues:   */
    /***********************************************************/

    case N_Compilation_Unit:

      /* This is not called for the main unit, which is handled in function
	 gigi above.  */
      start_stmt_group ();
      gnat_pushlevel ();

      Compilation_Unit_to_gnu (gnat_node);
      gnu_result = alloc_stmt_list ();
      break;

    case N_Subprogram_Body_Stub:
    case N_Package_Body_Stub:
    case N_Protected_Body_Stub:
    case N_Task_Body_Stub:
      /* Simply process whatever unit is being inserted.  */
      gnu_result = gnat_to_gnu (Unit (Library_Unit (gnat_node)));
      break;

    case N_Subunit:
      gnu_result = gnat_to_gnu (Proper_Body (gnat_node));
      break;

    /***************************/
    /* Chapter 11: Exceptions: */
    /***************************/

    case N_Handled_Sequence_Of_Statements:
      /* If there is an At_End procedure attached to this node, and the EH
	 mechanism is SJLJ, we must have at least a corresponding At_End
	 handler, unless the No_Exception_Handlers restriction is set.  */
      gcc_assert (type_annotate_only
		  || Exception_Mechanism != Setjmp_Longjmp
		  || No (At_End_Proc (gnat_node))
		  || Present (Exception_Handlers (gnat_node))
		  || No_Exception_Handlers_Set ());

      gnu_result = Handled_Sequence_Of_Statements_to_gnu (gnat_node);
      break;

    case N_Exception_Handler:
      if (Exception_Mechanism == Setjmp_Longjmp)
	gnu_result = Exception_Handler_to_gnu_sjlj (gnat_node);
      else if (Exception_Mechanism == Back_End_Exceptions)
	gnu_result = Exception_Handler_to_gnu_zcx (gnat_node);
      else
	gcc_unreachable ();

      break;

    /*******************************/
    /* Chapter 12: Generic Units:  */
    /*******************************/

    case N_Generic_Function_Renaming_Declaration:
    case N_Generic_Package_Renaming_Declaration:
    case N_Generic_Procedure_Renaming_Declaration:
    case N_Generic_Package_Declaration:
    case N_Generic_Subprogram_Declaration:
    case N_Package_Instantiation:
    case N_Procedure_Instantiation:
    case N_Function_Instantiation:
      /* These nodes can appear on a declaration list but there is nothing to
	 to be done with them.  */
      gnu_result = alloc_stmt_list ();
      break;

    /***************************************************/
    /* Chapter 13: Representation Clauses and	       */
    /*             Implementation-Dependent Features:  */
    /***************************************************/

    case N_Attribute_Definition_Clause:

      gnu_result = alloc_stmt_list ();

      /* The only one we need deal with is for 'Address.  For the others, SEM
	 puts the information elsewhere.  We need only deal with 'Address
	 if the object has a Freeze_Node (which it never will currently).  */
      if (Get_Attribute_Id (Chars (gnat_node)) != Attr_Address
	  || No (Freeze_Node (Entity (Name (gnat_node)))))
	break;

      /* Get the value to use as the address and save it as the
	 equivalent for GNAT_TEMP.  When the object is frozen,
	 gnat_to_gnu_entity will do the right thing. */
      save_gnu_tree (Entity (Name (gnat_node)),
		     gnat_to_gnu (Expression (gnat_node)), true);
      break;

    case N_Enumeration_Representation_Clause:
    case N_Record_Representation_Clause:
    case N_At_Clause:
      /* We do nothing with these.  SEM puts the information elsewhere.  */
      gnu_result = alloc_stmt_list ();
      break;

    case N_Code_Statement:
      if (!type_annotate_only)
	{
	  tree gnu_template = gnat_to_gnu (Asm_Template (gnat_node));
	  tree gnu_input_list = NULL_TREE, gnu_output_list = NULL_TREE;
	  tree gnu_clobber_list = NULL_TREE;
	  char *clobber;

	  /* First process inputs, then outputs, then clobbers.  */
	  Setup_Asm_Inputs (gnat_node);
	  while (Present (gnat_temp = Asm_Input_Value ()))
	    {
	      tree gnu_value = gnat_to_gnu (gnat_temp);
	      tree gnu_constr = build_tree_list (NULL_TREE, gnat_to_gnu
						 (Asm_Input_Constraint ()));

	      gnu_input_list
		= tree_cons (gnu_constr, gnu_value, gnu_input_list);
	      Next_Asm_Input ();
	    }

	  Setup_Asm_Outputs (gnat_node);
	  while (Present (gnat_temp = Asm_Output_Variable ()))
	    {
	      tree gnu_value = gnat_to_gnu (gnat_temp);
	      tree gnu_constr = build_tree_list (NULL_TREE, gnat_to_gnu
						 (Asm_Output_Constraint ()));

	      gnu_output_list
		= tree_cons (gnu_constr, gnu_value, gnu_output_list);
	      Next_Asm_Output ();
	    }

	  Clobber_Setup (gnat_node);
	  while ((clobber = Clobber_Get_Next ()))
	    gnu_clobber_list
	      = tree_cons (NULL_TREE,
			   build_string (strlen (clobber) + 1, clobber),
			   gnu_clobber_list);

	  gnu_input_list = nreverse (gnu_input_list);
	  gnu_output_list = nreverse (gnu_output_list);
	  gnu_result = build4 (ASM_EXPR,  void_type_node,
			       gnu_template, gnu_output_list,
			       gnu_input_list, gnu_clobber_list);
	  ASM_VOLATILE_P (gnu_result) = Is_Asm_Volatile (gnat_node);
	}
      else
	gnu_result = alloc_stmt_list ();

      break;

    /***************************************************/
    /* Added Nodes	                               */
    /***************************************************/

    case N_Freeze_Entity:
      start_stmt_group ();
      process_freeze_entity (gnat_node);
      process_decls (Actions (gnat_node), Empty, Empty, true, true);
      gnu_result = end_stmt_group ();
      break;

    case N_Itype_Reference:
      if (!present_gnu_tree (Itype (gnat_node)))
	process_type (Itype (gnat_node));

      gnu_result = alloc_stmt_list ();
      break;

    case N_Free_Statement:
      if (!type_annotate_only)
	{
	  tree gnu_ptr = gnat_to_gnu (Expression (gnat_node));
	  tree gnu_ptr_type = TREE_TYPE (gnu_ptr);
	  tree gnu_obj_type;
	  tree gnu_actual_obj_type = 0;
	  tree gnu_obj_size;
	  int align;

	  /* If this is a thin pointer, we must dereference it to create
	     a fat pointer, then go back below to a thin pointer.  The
	     reason for this is that we need a fat pointer someplace in
	     order to properly compute the size.  */
	  if (TYPE_THIN_POINTER_P (TREE_TYPE (gnu_ptr)))
	    gnu_ptr = build_unary_op (ADDR_EXPR, NULL_TREE,
				      build_unary_op (INDIRECT_REF, NULL_TREE,
						      gnu_ptr));

	  /* If this is an unconstrained array, we know the object must
	     have been allocated with the template in front of the object.
	     So pass the template address, but get the total size.  Do this
	     by converting to a thin pointer.  */
	  if (TYPE_FAT_POINTER_P (TREE_TYPE (gnu_ptr)))
	    gnu_ptr
	      = convert (build_pointer_type
			 (TYPE_OBJECT_RECORD_TYPE
			  (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (gnu_ptr)))),
			 gnu_ptr);

	  gnu_obj_type = TREE_TYPE (TREE_TYPE (gnu_ptr));

	  if (Present (Actual_Designated_Subtype (gnat_node)))
	    {
	      gnu_actual_obj_type = gnat_to_gnu_type (Actual_Designated_Subtype (gnat_node));

	      if (TYPE_FAT_OR_THIN_POINTER_P (gnu_ptr_type))
	        gnu_actual_obj_type
	          = build_unc_object_type_from_ptr (gnu_ptr_type,
		      gnu_actual_obj_type,
		      get_identifier ("DEALLOC"));
	    }
	  else
	    gnu_actual_obj_type = gnu_obj_type;

	  gnu_obj_size = TYPE_SIZE_UNIT (gnu_actual_obj_type);
	  align = TYPE_ALIGN (gnu_obj_type);

	  if (TREE_CODE (gnu_obj_type) == RECORD_TYPE
	      && TYPE_CONTAINS_TEMPLATE_P (gnu_obj_type))
	    {
	      tree gnu_char_ptr_type = build_pointer_type (char_type_node);
	      tree gnu_pos = byte_position (TYPE_FIELDS (gnu_obj_type));
	      tree gnu_byte_offset
		= convert (gnu_char_ptr_type,
			   size_diffop (size_zero_node, gnu_pos));

	      gnu_ptr = convert (gnu_char_ptr_type, gnu_ptr);
	      gnu_ptr = build_binary_op (MINUS_EXPR, gnu_char_ptr_type,
					 gnu_ptr, gnu_byte_offset);
	    }

	  gnu_result = build_call_alloc_dealloc (gnu_ptr, gnu_obj_size, align,
						 Procedure_To_Call (gnat_node),
						 Storage_Pool (gnat_node),
						 gnat_node);
	}
      break;

    case N_Raise_Constraint_Error:
    case N_Raise_Program_Error:
    case N_Raise_Storage_Error:
      if (type_annotate_only)
	{
	  gnu_result = alloc_stmt_list ();
	  break;
	}

      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result
	= build_call_raise (UI_To_Int (Reason (gnat_node)), gnat_node);

      /* If the type is VOID, this is a statement, so we need to
	 generate the code for the call.  Handle a Condition, if there
	 is one.  */
      if (TREE_CODE (gnu_result_type) == VOID_TYPE)
	{
	  annotate_with_node (gnu_result, gnat_node);

	  if (Present (Condition (gnat_node)))
	    gnu_result = build3 (COND_EXPR, void_type_node,
				 gnat_to_gnu (Condition (gnat_node)),
				 gnu_result, alloc_stmt_list ());
	}
      else
	gnu_result = build1 (NULL_EXPR, gnu_result_type, gnu_result);
      break;

    case N_Validate_Unchecked_Conversion:
      /* If the result is a pointer type, see if we are either converting
         from a non-pointer or from a pointer to a type with a different
 	 alias set and warn if so.  If the result defined in the same unit as
 	 this unchecked conversion, we can allow this because we can know to
 	 make that type have alias set 0.  */
      {
 	tree gnu_source_type = gnat_to_gnu_type (Source_Type (gnat_node));
 	tree gnu_target_type = gnat_to_gnu_type (Target_Type (gnat_node));

 	if (POINTER_TYPE_P (gnu_target_type)
 	    && !In_Same_Source_Unit (Target_Type (gnat_node), gnat_node)
            && get_alias_set (TREE_TYPE (gnu_target_type)) != 0
            && !No_Strict_Aliasing (Underlying_Type (Target_Type (gnat_node)))
 	    && (!POINTER_TYPE_P (gnu_source_type)
 		|| (get_alias_set (TREE_TYPE (gnu_source_type))
 		    != get_alias_set (TREE_TYPE (gnu_target_type)))))
 	  {
            post_error_ne
              ("?possible aliasing problem for type&",
               gnat_node, Target_Type (gnat_node));
	    post_error
              ("\\?use -fno-strict-aliasing switch for references",
               gnat_node);
	    post_error_ne
              ("\\?or use `pragma No_Strict_Aliasing (&);`",
               gnat_node, Target_Type (gnat_node));
	  }

	/* The No_Strict_Aliasing flag is not propagated to the back-end for
	   fat pointers so unconditionally warn in problematic cases.  */
	else if (TYPE_FAT_POINTER_P (gnu_target_type))
	  {
	    tree array_type
	      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_target_type)));

	    if (get_alias_set (array_type) != 0
		&& (!TYPE_FAT_POINTER_P (gnu_source_type)
		    || (get_alias_set (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_source_type))))
			!= get_alias_set (array_type))))
	      {
		post_error_ne
		  ("?possible aliasing problem for type&",
		   gnat_node, Target_Type (gnat_node));
		post_error
		  ("\\?use -fno-strict-aliasing switch for references",
		   gnat_node);
	      }
	  }
      }
      gnu_result = alloc_stmt_list ();
      break;

    case N_Raise_Statement:
    case N_Function_Specification:
    case N_Procedure_Specification:
    case N_Op_Concat:
    case N_Component_Association:
    case N_Task_Body:
    default:
      gcc_assert (type_annotate_only);
      gnu_result = alloc_stmt_list ();
    }

  /* If we pushed our level as part of processing the elaboration routine,
     pop it back now.  */
  if (went_into_elab_proc)
    {
      add_stmt (gnu_result);
      gnat_poplevel ();
      gnu_result = end_stmt_group ();
      current_function_decl = NULL_TREE;
    }

  /* Set the location information into the result.  Note that we may have
     no result if we tried to build a CALL_EXPR node to a procedure with
     no side-effects and optimization is enabled.  */
  if (gnu_result && EXPR_P (gnu_result))
    annotate_with_node (gnu_result, gnat_node);

  /* If we're supposed to return something of void_type, it means we have
     something we're elaborating for effect, so just return.  */
  if (TREE_CODE (gnu_result_type) == VOID_TYPE)
    return gnu_result;

  /* If the result is a constant that overflows, raise constraint error.  */
  else if (TREE_CODE (gnu_result) == INTEGER_CST
      && TREE_CONSTANT_OVERFLOW (gnu_result))
    {
      post_error ("Constraint_Error will be raised at run-time?", gnat_node);

      gnu_result
	= build1 (NULL_EXPR, gnu_result_type,
		  build_call_raise (CE_Overflow_Check_Failed, gnat_node));
    }

  /* If our result has side-effects and is of an unconstrained type,
     make a SAVE_EXPR so that we can be sure it will only be referenced
     once.  Note we must do this before any conversions.  */
  if (TREE_SIDE_EFFECTS (gnu_result)
      && (TREE_CODE (gnu_result_type) == UNCONSTRAINED_ARRAY_TYPE
	  || CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_result_type))))
    gnu_result = gnat_stabilize_reference (gnu_result, false);

  /* Now convert the result to the proper type.  If the type is void or if
     we have no result, return error_mark_node to show we have no result.
     If the type of the result is correct or if we have a label (which doesn't
     have any well-defined type), return our result.  Also don't do the
     conversion if the "desired" type involves a PLACEHOLDER_EXPR in its size
     since those are the cases where the front end may have the type wrong due
     to "instantiating" the unconstrained record with discriminant values
     or if this is a FIELD_DECL.  If this is the Name of an assignment
     statement or a parameter of a procedure call, return what we have since
     the RHS has to be converted to our type there in that case, unless
     GNU_RESULT_TYPE has a simpler size.  Similarly, if the two types are
     record types with the same name, the expression type has integral mode,
     and GNU_RESULT_TYPE BLKmode, don't convert.  This will be the case when
     we are converting from a packable type to its actual type and we need
     those conversions to be NOPs in order for assignments into these types to
     work properly if the inner object is a bitfield and hence can't have
     its address taken.  Finally, don't convert integral types that are the
     operand of an unchecked conversion since we need to ignore those
     conversions (for 'Valid).  Otherwise, convert the result to the proper
     type.  */

  if (Present (Parent (gnat_node))
      && ((Nkind (Parent (gnat_node)) == N_Assignment_Statement
	   && Name (Parent (gnat_node)) == gnat_node)
	  || (Nkind (Parent (gnat_node)) == N_Procedure_Call_Statement
	      && Name (Parent (gnat_node)) != gnat_node)
	  || (Nkind (Parent (gnat_node)) == N_Unchecked_Type_Conversion
	      && !AGGREGATE_TYPE_P (gnu_result_type)
	      && !AGGREGATE_TYPE_P (TREE_TYPE (gnu_result)))
	  || Nkind (Parent (gnat_node)) == N_Parameter_Association)
      && !(TYPE_SIZE (gnu_result_type)
	   && TYPE_SIZE (TREE_TYPE (gnu_result))
	   && (AGGREGATE_TYPE_P (gnu_result_type)
	       == AGGREGATE_TYPE_P (TREE_TYPE (gnu_result)))
	   && ((TREE_CODE (TYPE_SIZE (gnu_result_type)) == INTEGER_CST
		&& (TREE_CODE (TYPE_SIZE (TREE_TYPE (gnu_result)))
		    != INTEGER_CST))
	       || (TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
		   && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_result_type))
		   && (CONTAINS_PLACEHOLDER_P
		       (TYPE_SIZE (TREE_TYPE (gnu_result))))))
	   && !(TREE_CODE (gnu_result_type) == RECORD_TYPE
		&& TYPE_JUSTIFIED_MODULAR_P (gnu_result_type))))
    {
      /* In this case remove padding only if the inner object is of
	 self-referential size: in that case it must be an object of
	 unconstrained type with a default discriminant.  In other cases,
	 we want to avoid copying too much data.  */
      if (TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_result))
	  && CONTAINS_PLACEHOLDER_P (TYPE_SIZE
				     (TREE_TYPE (TYPE_FIELDS
						 (TREE_TYPE (gnu_result))))))
	gnu_result = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			      gnu_result);
    }

  else if (TREE_CODE (gnu_result) == LABEL_DECL
	   || TREE_CODE (gnu_result) == FIELD_DECL
	   || TREE_CODE (gnu_result) == ERROR_MARK
	   || (TYPE_SIZE (gnu_result_type)
	       && TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
	       && TREE_CODE (gnu_result) != INDIRECT_REF
	       && CONTAINS_PLACEHOLDER_P (TYPE_SIZE (gnu_result_type)))
	   || ((TYPE_NAME (gnu_result_type)
		== TYPE_NAME (TREE_TYPE (gnu_result)))
	       && TREE_CODE (gnu_result_type) == RECORD_TYPE
	       && TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
	       && TYPE_MODE (gnu_result_type) == BLKmode
	       && (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (gnu_result)))
		   == MODE_INT)))
    {
      /* Remove any padding record, but do nothing more in this case.  */
      if (TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
	gnu_result = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			      gnu_result);
    }

  else if (gnu_result == error_mark_node
	   || gnu_result_type == void_type_node)
    gnu_result =  error_mark_node;
  else if (gnu_result_type != TREE_TYPE (gnu_result))
    gnu_result = convert (gnu_result_type, gnu_result);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RESULT.  */
  while ((TREE_CODE (gnu_result) == NOP_EXPR
	  || TREE_CODE (gnu_result) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_result, 0)) == TREE_TYPE (gnu_result))
    gnu_result = TREE_OPERAND (gnu_result, 0);

  return gnu_result;
}

/* Record the current code position in GNAT_NODE.  */

static void
record_code_position (Node_Id gnat_node)
{
  tree stmt_stmt = build1 (STMT_STMT, void_type_node, NULL_TREE);

  add_stmt_with_node (stmt_stmt, gnat_node);
  save_gnu_tree (gnat_node, stmt_stmt, true);
}

/* Insert the code for GNAT_NODE at the position saved for that node.  */

static void
insert_code_for (Node_Id gnat_node)
{
  STMT_STMT_STMT (get_gnu_tree (gnat_node)) = gnat_to_gnu (gnat_node);
  save_gnu_tree (gnat_node, NULL_TREE, true);
}

/* Start a new statement group chained to the previous group.  */

static void
start_stmt_group ()
{
  struct stmt_group *group = stmt_group_free_list;

  /* First see if we can get one from the free list.  */
  if (group)
    stmt_group_free_list = group->previous;
  else
    group = (struct stmt_group *) ggc_alloc (sizeof (struct stmt_group));

  group->previous = current_stmt_group;
  group->stmt_list = group->block = group->cleanups = NULL_TREE;
  current_stmt_group = group;
}

/* Add GNU_STMT to the current statement group.  */

void
add_stmt (tree gnu_stmt)
{
  append_to_statement_list (gnu_stmt, &current_stmt_group->stmt_list);

  /* If we're at top level, show everything in here is in use in case
     any of it is shared by a subprogram.  */
  if (global_bindings_p ())
    walk_tree (&gnu_stmt, mark_visited, NULL, NULL);

}

/* Similar, but set the location of GNU_STMT to that of GNAT_NODE.  */

void
add_stmt_with_node (tree gnu_stmt, Node_Id gnat_node)
{
  if (Present (gnat_node))
    annotate_with_node (gnu_stmt, gnat_node);
  add_stmt (gnu_stmt);
}

/* Add a declaration statement for GNU_DECL to the current statement group.
   Get SLOC from Entity_Id.  */

void
add_decl_expr (tree gnu_decl, Entity_Id gnat_entity)
{
  tree gnu_stmt;

  /* If this is a variable that Gigi is to ignore, we may have been given
     an ERROR_MARK.  So test for it.  We also might have been given a
     reference for a renaming.  So only do something for a decl.  Also
     ignore a TYPE_DECL for an UNCONSTRAINED_ARRAY_TYPE.  */
  if (!DECL_P (gnu_decl)
      || (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TREE_CODE (TREE_TYPE (gnu_decl)) == UNCONSTRAINED_ARRAY_TYPE))
    return;

  /* If we are global, we don't want to actually output the DECL_EXPR for
     this decl since we already have evaluated the expressions in the
     sizes and positions as globals and doing it again would be wrong.
     But we do have to mark everything as used.  */
  gnu_stmt = build1 (DECL_EXPR, void_type_node, gnu_decl);
  if (!global_bindings_p ())
    add_stmt_with_node (gnu_stmt, gnat_entity);
  else
    {
      walk_tree (&gnu_stmt, mark_visited, NULL, NULL);
      if (TREE_CODE (gnu_decl) == VAR_DECL
	  || TREE_CODE (gnu_decl) == CONST_DECL)
	{
	  walk_tree (&DECL_SIZE (gnu_decl), mark_visited, NULL, NULL);
	  walk_tree (&DECL_SIZE_UNIT (gnu_decl), mark_visited, NULL, NULL);
	  walk_tree (&DECL_INITIAL (gnu_decl), mark_visited, NULL, NULL);
	}
    }

  /* If this is a DECL_EXPR for a variable with DECL_INITIAL set,
     there are two cases we need to handle here.  */
  if (TREE_CODE (gnu_decl) == VAR_DECL && DECL_INITIAL (gnu_decl))
    {
      tree gnu_init = DECL_INITIAL (gnu_decl);
      tree gnu_lhs = NULL_TREE;

      /* If this is a DECL_EXPR for a variable with DECL_INITIAL set
	 and decl has a padded type, convert it to the unpadded type so the
	 assignment is done properly.  */
      if (TREE_CODE (TREE_TYPE (gnu_decl)) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_decl)))
	gnu_lhs
	  = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_decl))), gnu_decl);

      /* Otherwise, if this is going into memory and the initializer isn't
	 valid for the assembler and loader.  Gimplification could do this,
	 but would be run too late if -fno-unit-at-a-time.  */
      else if (TREE_STATIC (gnu_decl)
	       && !initializer_constant_valid_p (gnu_init,
						 TREE_TYPE (gnu_decl)))
	gnu_lhs = gnu_decl;

      if (gnu_lhs)
	{
	  tree gnu_assign_stmt
	    = build_binary_op (MODIFY_EXPR, NULL_TREE,
			       gnu_lhs, DECL_INITIAL (gnu_decl));

	  DECL_INITIAL (gnu_decl) = 0;
	  TREE_READONLY (gnu_decl) = 0;
	  annotate_with_locus (gnu_assign_stmt,
			       DECL_SOURCE_LOCATION (gnu_decl));
	  add_stmt (gnu_assign_stmt);
	}
    }
}

/* Utility function to mark nodes with TREE_VISITED and types as having their
   sized gimplified.  Called from walk_tree.  We use this to indicate all
   variable sizes and positions in global types may not be shared by any
   subprogram.  */

static tree
mark_visited (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  if (TREE_VISITED (*tp))
    *walk_subtrees = 0;

  /* Don't mark a dummy type as visited because we want to mark its sizes
     and fields once it's filled in.  */
  else if (!TYPE_IS_DUMMY_P (*tp))
    TREE_VISITED (*tp) = 1;

  if (TYPE_P (*tp))
    TYPE_SIZES_GIMPLIFIED (*tp) = 1;

  return NULL_TREE;
}

/* Likewise, but to mark as unvisited.  */

static tree
mark_unvisited (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
		void *data ATTRIBUTE_UNUSED)
{
  TREE_VISITED (*tp) = 0;

  return NULL_TREE;
}

/* Add GNU_CLEANUP, a cleanup action, to the current code group.  */

static void
add_cleanup (tree gnu_cleanup)
{
  append_to_statement_list (gnu_cleanup, &current_stmt_group->cleanups);
}

/* Set the BLOCK node corresponding to the current code group to GNU_BLOCK.  */

void
set_block_for_group (tree gnu_block)
{
  gcc_assert (!current_stmt_group->block);
  current_stmt_group->block = gnu_block;
}

/* Return code corresponding to the current code group.  It is normally
   a STATEMENT_LIST, but may also be a BIND_EXPR or TRY_FINALLY_EXPR if
   BLOCK or cleanups were set.  */

static tree
end_stmt_group ()
{
  struct stmt_group *group = current_stmt_group;
  tree gnu_retval = group->stmt_list;

  /* If this is a null list, allocate a new STATEMENT_LIST.  Then, if there
     are cleanups, make a TRY_FINALLY_EXPR.  Last, if there is a BLOCK,
     make a BIND_EXPR.  Note that we nest in that because the cleanup may
     reference variables in the block.  */
  if (gnu_retval == NULL_TREE)
    gnu_retval = alloc_stmt_list ();

  if (group->cleanups)
    gnu_retval = build2 (TRY_FINALLY_EXPR, void_type_node, gnu_retval,
			 group->cleanups);

  if (current_stmt_group->block)
    gnu_retval = build3 (BIND_EXPR, void_type_node, BLOCK_VARS (group->block),
			 gnu_retval, group->block);

  /* Remove this group from the stack and add it to the free list.  */
  current_stmt_group = group->previous;
  group->previous = stmt_group_free_list;
  stmt_group_free_list = group;

  return gnu_retval;
}

/* Add a list of statements from GNAT_LIST, a possibly-empty list of
   statements.*/

static void
add_stmt_list (List_Id gnat_list)
{
  Node_Id gnat_node;

  if (Present (gnat_list))
    for (gnat_node = First (gnat_list); Present (gnat_node);
	 gnat_node = Next (gnat_node))
      add_stmt (gnat_to_gnu (gnat_node));
}

/* Build a tree from GNAT_LIST, a possibly-empty list of statements.
   If BINDING_P is true, push and pop a binding level around the list.  */

static tree
build_stmt_group (List_Id gnat_list, bool binding_p)
{
  start_stmt_group ();
  if (binding_p)
    gnat_pushlevel ();

  add_stmt_list (gnat_list);
  if (binding_p)
    gnat_poplevel ();

  return end_stmt_group ();
}

/* Push and pop routines for stacks.  We keep a free list around so we
   don't waste tree nodes.  */

static void
push_stack (tree *gnu_stack_ptr, tree gnu_purpose, tree gnu_value)
{
  tree gnu_node = gnu_stack_free_list;

  if (gnu_node)
    {
      gnu_stack_free_list = TREE_CHAIN (gnu_node);
      TREE_CHAIN (gnu_node) = *gnu_stack_ptr;
      TREE_PURPOSE (gnu_node) = gnu_purpose;
      TREE_VALUE (gnu_node) = gnu_value;
    }
  else
    gnu_node = tree_cons (gnu_purpose, gnu_value, *gnu_stack_ptr);

  *gnu_stack_ptr = gnu_node;
}

static void
pop_stack (tree *gnu_stack_ptr)
{
  tree gnu_node = *gnu_stack_ptr;

  *gnu_stack_ptr = TREE_CHAIN (gnu_node);
  TREE_CHAIN (gnu_node) = gnu_stack_free_list;
  gnu_stack_free_list = gnu_node;
}

/* GNU_STMT is a statement.  We generate code for that statement.  */

void
gnat_expand_stmt (tree gnu_stmt)
{
#if 0
  tree gnu_elmt, gnu_elmt_2;
#endif

  switch (TREE_CODE (gnu_stmt))
    {
#if 0
    case USE_STMT:
      /* First write a volatile ASM_INPUT to prevent anything from being
	 moved.  */
      gnu_elmt = gen_rtx_ASM_INPUT (VOIDmode, "");
      MEM_VOLATILE_P (gnu_elmt) = 1;
      emit_insn (gnu_elmt);

      gnu_elmt = expand_expr (TREE_OPERAND (gnu_stmt, 0), NULL_RTX, VOIDmode,
			    modifier);
      emit_insn (gen_rtx_USE (VOIDmode, ));
      return target;
#endif

    default:
      gcc_unreachable ();
    }
}

/* Generate GIMPLE in place for the expression at *EXPR_P.  */

int
gnat_gimplify_expr (tree *expr_p, tree *pre_p, tree *post_p ATTRIBUTE_UNUSED)
{
  tree expr = *expr_p;
  tree op;

  if (IS_ADA_STMT (expr))
    return gnat_gimplify_stmt (expr_p);

  switch (TREE_CODE (expr))
    {
    case NULL_EXPR:
      /* If this is for a scalar, just make a VAR_DECL for it.  If for
	 an aggregate, get a null pointer of the appropriate type and
	 dereference it.  */
      if (AGGREGATE_TYPE_P (TREE_TYPE (expr)))
	*expr_p = build1 (INDIRECT_REF, TREE_TYPE (expr),
			  convert (build_pointer_type (TREE_TYPE (expr)),
				   integer_zero_node));
      else
	{
	  *expr_p = create_tmp_var (TREE_TYPE (expr), NULL);
	  TREE_NO_WARNING (*expr_p) = 1;
	}

      append_to_statement_list (TREE_OPERAND (expr, 0), pre_p);
      return GS_OK;

    case UNCONSTRAINED_ARRAY_REF:
      /* We should only do this if we are just elaborating for side-effects,
	 but we can't know that yet.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      return GS_OK;

    case ADDR_EXPR:
      op = TREE_OPERAND (expr, 0);

      /* If we're taking the address of a constant CONSTRUCTOR, force it to
	 be put into static memory.  We know it's going to be readonly given
	 the semantics we have and it's required to be static memory in
	 the case when the reference is in an elaboration procedure.   */
      if (TREE_CODE (op) == CONSTRUCTOR && TREE_CONSTANT (op))
	{
	  tree new_var = create_tmp_var (TREE_TYPE (op), "C");

	  TREE_READONLY (new_var) = 1;
	  TREE_STATIC (new_var) = 1;
	  TREE_ADDRESSABLE (new_var) = 1;
	  DECL_INITIAL (new_var) = op;

	  TREE_OPERAND (expr, 0) = new_var;
	  recompute_tree_invarant_for_addr_expr (expr);
	  return GS_ALL_DONE;
	}

      /* Otherwise, if we are taking the address of something that is neither
	 reference, declaration, or constant, make a variable for the operand
	 here and then take its address.  If we don't do it this way, we may
	 confuse the gimplifier because it needs to know the variable is
	 addressable at this point.  This duplicates code in
	 internal_get_tmp_var, which is unfortunate.  */
      else if (TREE_CODE_CLASS (TREE_CODE (op)) != tcc_reference
	       && TREE_CODE_CLASS (TREE_CODE (op)) != tcc_declaration
	       && TREE_CODE_CLASS (TREE_CODE (op)) != tcc_constant)
	{
	  tree new_var = create_tmp_var (TREE_TYPE (op), "A");
	  tree mod = build2 (MODIFY_EXPR, TREE_TYPE (op), new_var, op);

	  TREE_ADDRESSABLE (new_var) = 1;

	  if (EXPR_HAS_LOCATION (op))
	    SET_EXPR_LOCUS (mod, EXPR_LOCUS (op));

	  gimplify_and_add (mod, pre_p);
	  TREE_OPERAND (expr, 0) = new_var;
	  recompute_tree_invarant_for_addr_expr (expr);
	  return GS_ALL_DONE;
	}

      return GS_UNHANDLED;

    case COMPONENT_REF:
      /* We have a kludge here.  If the FIELD_DECL is from a fat pointer and is
	 from an early dummy type, replace it with the proper FIELD_DECL.  */
      if (TYPE_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (*expr_p, 0)))
	  && DECL_ORIGINAL_FIELD (TREE_OPERAND (*expr_p, 1)))
	{
	  TREE_OPERAND (*expr_p, 1)
	    = DECL_ORIGINAL_FIELD (TREE_OPERAND (*expr_p, 1));
	  return GS_OK;
	}

      /* ... fall through ... */

    default:
      return GS_UNHANDLED;
    }
}

/* Generate GIMPLE in place for the statement at *STMT_P.  */

static enum gimplify_status
gnat_gimplify_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;

  switch (TREE_CODE (stmt))
    {
    case STMT_STMT:
      *stmt_p = STMT_STMT_STMT (stmt);
      return GS_OK;

    case USE_STMT:
      *stmt_p = NULL_TREE;
      return GS_ALL_DONE;

    case LOOP_STMT:
      {
	tree gnu_start_label = create_artificial_label ();
	tree gnu_end_label = LOOP_STMT_LABEL (stmt);

	/* Set to emit the statements of the loop.  */
	*stmt_p = NULL_TREE;

	/* We first emit the start label and then a conditional jump to
	   the end label if there's a top condition, then the body of the
	   loop, then a conditional branch to the end label, then the update,
	   if any, and finally a jump to the start label and the definition
	   of the end label.  */
	append_to_statement_list (build1 (LABEL_EXPR, void_type_node,
					  gnu_start_label),
				  stmt_p);

	if (LOOP_STMT_TOP_COND (stmt))
	  append_to_statement_list (build3 (COND_EXPR, void_type_node,
					    LOOP_STMT_TOP_COND (stmt),
					    alloc_stmt_list (),
					    build1 (GOTO_EXPR,
						    void_type_node,
						    gnu_end_label)),
				    stmt_p);

	append_to_statement_list (LOOP_STMT_BODY (stmt), stmt_p);

	if (LOOP_STMT_BOT_COND (stmt))
	  append_to_statement_list (build3 (COND_EXPR, void_type_node,
					    LOOP_STMT_BOT_COND (stmt),
					    alloc_stmt_list (),
					    build1 (GOTO_EXPR,
						    void_type_node,
						    gnu_end_label)),
				    stmt_p);

	if (LOOP_STMT_UPDATE (stmt))
	  append_to_statement_list (LOOP_STMT_UPDATE (stmt), stmt_p);

	append_to_statement_list (build1 (GOTO_EXPR, void_type_node,
					  gnu_start_label),
				  stmt_p);
	append_to_statement_list (build1 (LABEL_EXPR, void_type_node,
					  gnu_end_label),
				  stmt_p);
	return GS_OK;
      }

    case EXIT_STMT:
      /* Build a statement to jump to the corresponding end label, then
	 see if it needs to be conditional.  */
      *stmt_p = build1 (GOTO_EXPR, void_type_node, EXIT_STMT_LABEL (stmt));
      if (EXIT_STMT_COND (stmt))
	*stmt_p = build3 (COND_EXPR, void_type_node,
			  EXIT_STMT_COND (stmt), *stmt_p, alloc_stmt_list ());
      return GS_OK;

    default:
      gcc_unreachable ();
    }
}

/* Force references to each of the entities in packages GNAT_NODE with's
   so that the debugging information for all of them are identical
   in all clients.  Operate recursively on anything it with's, but check
   that we aren't elaborating something more than once.  */

/* The reason for this routine's existence is two-fold.
   First, with some debugging formats, notably MDEBUG on SGI
   IRIX, the linker will remove duplicate debugging information if two
   clients have identical debugguing information.  With the normal scheme
   of elaboration, this does not usually occur, since entities in with'ed
   packages are elaborated on demand, and if clients have different usage
   patterns, the normal case, then the order and selection of entities
   will differ.  In most cases however, it seems that linkers do not know
   how to eliminate duplicate debugging information, even if it is
   identical, so the use of this routine would increase the total amount
   of debugging information in the final executable.

   Second, this routine is called in type_annotate mode, to compute DDA
   information for types in withed units, for ASIS use  */

static void
elaborate_all_entities (Node_Id gnat_node)
{
  Entity_Id gnat_with_clause, gnat_entity;

  /* Process each unit only once. As we trace the context of all relevant
     units transitively, including generic bodies, we may encounter the
     same generic unit repeatedly */

  if (!present_gnu_tree (gnat_node))
     save_gnu_tree (gnat_node, integer_zero_node, true);

  /* Save entities in all context units. A body may have an implicit_with
     on its own spec, if the context includes a child unit, so don't save
     the spec twice.  */

  for (gnat_with_clause = First (Context_Items (gnat_node));
       Present (gnat_with_clause);
       gnat_with_clause = Next (gnat_with_clause))
    if (Nkind (gnat_with_clause) == N_With_Clause
	&& !present_gnu_tree (Library_Unit (gnat_with_clause))
        && Library_Unit (gnat_with_clause) != Library_Unit (Cunit (Main_Unit)))
      {
	elaborate_all_entities (Library_Unit (gnat_with_clause));

	if (Ekind (Entity (Name (gnat_with_clause))) == E_Package)
	  {
	    for (gnat_entity = First_Entity (Entity (Name (gnat_with_clause)));
		 Present (gnat_entity);
		 gnat_entity = Next_Entity (gnat_entity))
	      if (Is_Public (gnat_entity)
		  && Convention (gnat_entity) != Convention_Intrinsic
		  && Ekind (gnat_entity) != E_Package
		  && Ekind (gnat_entity) != E_Package_Body
		  && Ekind (gnat_entity) != E_Operator
		  && !(IN (Ekind (gnat_entity), Type_Kind)
		       && !Is_Frozen (gnat_entity))
		  && !((Ekind (gnat_entity) == E_Procedure
			|| Ekind (gnat_entity) == E_Function)
		       && Is_Intrinsic_Subprogram (gnat_entity))
		  && !IN (Ekind (gnat_entity), Named_Kind)
		  && !IN (Ekind (gnat_entity), Generic_Unit_Kind))
		gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
          }
        else if (Ekind (Entity (Name (gnat_with_clause))) == E_Generic_Package)
           {
            Node_Id gnat_body
	      = Corresponding_Body (Unit (Library_Unit (gnat_with_clause)));

            /* Retrieve compilation unit node of generic body.  */
            while (Present (gnat_body)
		   && Nkind (gnat_body) != N_Compilation_Unit)
	      gnat_body = Parent (gnat_body);

            /* If body is available, elaborate its context.  */
            if (Present (gnat_body))
                elaborate_all_entities (gnat_body);
           }
      }

  if (Nkind (Unit (gnat_node)) == N_Package_Body && type_annotate_only)
    elaborate_all_entities (Library_Unit (gnat_node));
}

/* Do the processing of N_Freeze_Entity, GNAT_NODE.  */

static void
process_freeze_entity (Node_Id gnat_node)
{
  Entity_Id gnat_entity = Entity (gnat_node);
  tree gnu_old;
  tree gnu_new;
  tree gnu_init
    = (Nkind (Declaration_Node (gnat_entity)) == N_Object_Declaration
       && present_gnu_tree (Declaration_Node (gnat_entity)))
      ? get_gnu_tree (Declaration_Node (gnat_entity)) : NULL_TREE;

  /* If this is a package, need to generate code for the package.  */
  if (Ekind (gnat_entity) == E_Package)
    {
      insert_code_for
  	(Parent (Corresponding_Body
  		 (Parent (Declaration_Node (gnat_entity)))));
      return;
    }

  /* Check for old definition after the above call.  This Freeze_Node
     might be for one its Itypes.  */
  gnu_old
    = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;

  /* If this entity has an Address representation clause, GNU_OLD is the
     address, so discard it here.  */
  if (Present (Address_Clause (gnat_entity)))
    gnu_old = 0;

  /* Don't do anything for class-wide types they are always
     transformed into their root type.  */
  if (Ekind (gnat_entity) == E_Class_Wide_Type
      || (Ekind (gnat_entity) == E_Class_Wide_Subtype
	  && Present (Equivalent_Type (gnat_entity))))
    return;

  /* Don't do anything for subprograms that may have been elaborated before
     their freeze nodes.  This can happen, for example because of an inner call
     in an instance body, or a previous compilation of a spec for inlining
     purposes.  */
  if  ((gnu_old
        && TREE_CODE (gnu_old) == FUNCTION_DECL
        && (Ekind (gnat_entity) == E_Function
          || Ekind (gnat_entity) == E_Procedure))
    || (gnu_old
        && (TREE_CODE (TREE_TYPE (gnu_old)) == FUNCTION_TYPE
        && Ekind (gnat_entity) == E_Subprogram_Type)))
    return;

  /* If we have a non-dummy type old tree, we have nothing to do.   Unless
     this is the public view of a private type whose full view was not
     delayed, this node was never delayed as it should have been.
     Also allow this to happen for concurrent types since we may have
     frozen both the Corresponding_Record_Type and this type.  */
  if (gnu_old
      && !(TREE_CODE (gnu_old) == TYPE_DECL
	   && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old))))
    {
      gcc_assert ((IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
		   && Present (Full_View (gnat_entity))
		   && No (Freeze_Node (Full_View (gnat_entity))))
		  || Is_Concurrent_Type (gnat_entity));
      return;
    }

  /* Reset the saved tree, if any, and elaborate the object or type for real.
     If there is a full declaration, elaborate it and copy the type to
     GNAT_ENTITY.  Likewise if this is the record subtype corresponding to
     a class wide type or subtype.  */
  if (gnu_old)
    {
      save_gnu_tree (gnat_entity, NULL_TREE, false);
      if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
  	  && Present (Full_View (gnat_entity))
  	  && present_gnu_tree (Full_View (gnat_entity)))
  	save_gnu_tree (Full_View (gnat_entity), NULL_TREE, false);
      if (Present (Class_Wide_Type (gnat_entity))
	  && Class_Wide_Type (gnat_entity) != gnat_entity)
	save_gnu_tree (Class_Wide_Type (gnat_entity), NULL_TREE, false);
    }

  if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
      && Present (Full_View (gnat_entity)))
    {
      gnu_new = gnat_to_gnu_entity (Full_View (gnat_entity), NULL_TREE, 1);

      /* Propagate back-annotations from full view to partial view.  */
      if (Unknown_Alignment (gnat_entity))
	Set_Alignment (gnat_entity, Alignment (Full_View (gnat_entity)));

      if (Unknown_Esize (gnat_entity))
	Set_Esize (gnat_entity, Esize (Full_View (gnat_entity)));

      if (Unknown_RM_Size (gnat_entity))
	Set_RM_Size (gnat_entity, RM_Size (Full_View (gnat_entity)));

      /* The above call may have defined this entity (the simplest example
  	 of this is when we have a private enumeral type since the bounds
  	 will have the public view.  */
      if (!present_gnu_tree (gnat_entity))
  	save_gnu_tree (gnat_entity, gnu_new, false);
      if (Present (Class_Wide_Type (gnat_entity))
	  && Class_Wide_Type (gnat_entity) != gnat_entity)
	save_gnu_tree (Class_Wide_Type (gnat_entity), gnu_new, false);
    }
  else
    gnu_new = gnat_to_gnu_entity (gnat_entity, gnu_init, 1);

  /* If we've made any pointers to the old version of this type, we
     have to update them.  */
  if (gnu_old)
    update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_old)),
		       TREE_TYPE (gnu_new));
}

/* Process the list of inlined subprograms of GNAT_NODE, which is an
   N_Compilation_Unit.  */

static void
process_inlined_subprograms (Node_Id gnat_node)
{
  Entity_Id gnat_entity;
  Node_Id gnat_body;

  /* If we can inline, generate RTL for all the inlined subprograms.
     Define the entity first so we set DECL_EXTERNAL.  */
  if (optimize > 0 && !flag_really_no_inline)
    for (gnat_entity = First_Inlined_Subprogram (gnat_node);
	 Present (gnat_entity);
	 gnat_entity = Next_Inlined_Subprogram (gnat_entity))
      {
	gnat_body = Parent (Declaration_Node (gnat_entity));

	if (Nkind (gnat_body) != N_Subprogram_Body)
	  {
	    /* ??? This really should always be Present.  */
	    if (No (Corresponding_Body (gnat_body)))
	      continue;

	    gnat_body
	      = Parent (Declaration_Node (Corresponding_Body (gnat_body)));
	  }

	if (Present (gnat_body))
	  {
	    gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
	    add_stmt (gnat_to_gnu (gnat_body));
	  }
      }
}

/* Elaborate decls in the lists GNAT_DECLS and GNAT_DECLS2, if present.
   We make two passes, one to elaborate anything other than bodies (but
   we declare a function if there was no spec).  The second pass
   elaborates the bodies.

   GNAT_END_LIST gives the element in the list past the end.  Normally,
   this is Empty, but can be First_Real_Statement for a
   Handled_Sequence_Of_Statements.

   We make a complete pass through both lists if PASS1P is true, then make
   the second pass over both lists if PASS2P is true.  The lists usually
   correspond to the public and private parts of a package.  */

static void
process_decls (List_Id gnat_decls, List_Id gnat_decls2,
               Node_Id gnat_end_list, bool pass1p, bool pass2p)
{
  List_Id gnat_decl_array[2];
  Node_Id gnat_decl;
  int i;

  gnat_decl_array[0] = gnat_decls, gnat_decl_array[1] = gnat_decls2;

  if (pass1p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    /* For package specs, we recurse inside the declarations,
	       thus taking the two pass approach inside the boundary.  */
	    if (Nkind (gnat_decl) == N_Package_Declaration
		&& (Nkind (Specification (gnat_decl)
			   == N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, true, false);

	    /* Similarly for any declarations in the actions of a
	       freeze node.  */
	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      {
		process_freeze_entity (gnat_decl);
		process_decls (Actions (gnat_decl), Empty, Empty, true, false);
	      }

	    /* Package bodies with freeze nodes get their elaboration deferred
	       until the freeze node, but the code must be placed in the right
	       place, so record the code position now.  */
	    else if (Nkind (gnat_decl) == N_Package_Body
		     && Present (Freeze_Node (Corresponding_Spec (gnat_decl))))
	      record_code_position (gnat_decl);

            else if (Nkind (gnat_decl) == N_Package_Body_Stub
		     && Present (Library_Unit (gnat_decl))
		     && Present (Freeze_Node
				 (Corresponding_Spec
				  (Proper_Body (Unit
						(Library_Unit (gnat_decl)))))))
	      record_code_position
		(Proper_Body (Unit (Library_Unit (gnat_decl))));

	    /* We defer most subprogram bodies to the second pass.  */
	    else if (Nkind (gnat_decl) == N_Subprogram_Body)
	      {
		if (Acts_As_Spec (gnat_decl))
		  {
		    Node_Id gnat_subprog_id = Defining_Entity (gnat_decl);

		    if (Ekind (gnat_subprog_id) != E_Generic_Procedure
			&& Ekind (gnat_subprog_id) != E_Generic_Function)
		      gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, 1);
		  }
	      }
            /* For bodies and stubs that act as their own specs, the entity
               itself must be elaborated in the first pass, because it may
               be used in other declarations. */
	    else if (Nkind (gnat_decl) == N_Subprogram_Body_Stub)
	      {
                  Node_Id gnat_subprog_id =
                     Defining_Entity (Specification (gnat_decl));

		    if (Ekind (gnat_subprog_id) != E_Subprogram_Body
                        && Ekind (gnat_subprog_id) != E_Generic_Procedure
			&& Ekind (gnat_subprog_id) != E_Generic_Function)
		      gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, 1);
               }

	    /* Concurrent stubs stand for the corresponding subprogram bodies,
	       which are deferred like other bodies.  */
	    else if (Nkind (gnat_decl) == N_Task_Body_Stub
		     || Nkind (gnat_decl) == N_Protected_Body_Stub)
	      ;
	    else
	      add_stmt (gnat_to_gnu (gnat_decl));
	  }

  /* Here we elaborate everything we deferred above except for package bodies,
     which are elaborated at their freeze nodes.  Note that we must also
     go inside things (package specs and freeze nodes) the first pass did.  */
  if (pass2p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    if (Nkind (gnat_decl) == N_Subprogram_Body
		|| Nkind (gnat_decl) == N_Subprogram_Body_Stub
		|| Nkind (gnat_decl) == N_Task_Body_Stub
		|| Nkind (gnat_decl) == N_Protected_Body_Stub)
	      add_stmt (gnat_to_gnu (gnat_decl));

	    else if (Nkind (gnat_decl) == N_Package_Declaration
		     && (Nkind (Specification (gnat_decl)
				== N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, false, true);

	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      process_decls (Actions (gnat_decl), Empty, Empty, false, true);
	  }
}

/* Emit code for a range check. GNU_EXPR is the expression to be checked,
   GNAT_RANGE_TYPE the gnat type or subtype containing the bounds against
   which we have to check. */

static tree
emit_range_check (tree gnu_expr, Entity_Id gnat_range_type)
{
  tree gnu_range_type = get_unpadded_type (gnat_range_type);
  tree gnu_low  = TYPE_MIN_VALUE (gnu_range_type);
  tree gnu_high = TYPE_MAX_VALUE (gnu_range_type);
  tree gnu_compare_type = get_base_type (TREE_TYPE (gnu_expr));

  /* If GNU_EXPR has an integral type that is narrower than GNU_RANGE_TYPE,
     we can't do anything since we might be truncating the bounds.  No
     check is needed in this case.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (gnu_expr))
      && (TYPE_PRECISION (gnu_compare_type)
	  < TYPE_PRECISION (get_base_type (gnu_range_type))))
    return gnu_expr;

  /* Checked expressions must be evaluated only once. */
  gnu_expr = protect_multiple_eval (gnu_expr);

  /* There's no good type to use here, so we might as well use
     integer_type_node. Note that the form of the check is
        (not (expr >= lo)) or (not (expr >= hi))
      the reason for this slightly convoluted form is that NaN's
      are not considered to be in range in the float case. */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      invert_truthvalue
		      (build_binary_op (GE_EXPR, integer_type_node,
				       convert (gnu_compare_type, gnu_expr),
				       convert (gnu_compare_type, gnu_low))),
		      invert_truthvalue
		      (build_binary_op (LE_EXPR, integer_type_node,
					convert (gnu_compare_type, gnu_expr),
					convert (gnu_compare_type,
						 gnu_high)))),
     gnu_expr, CE_Range_Check_Failed);
}

/* Emit code for an index check. GNU_ARRAY_OBJECT is the array object
   which we are about to index, GNU_EXPR is the index expression to be
   checked, GNU_LOW and GNU_HIGH are the lower and upper bounds
   against which GNU_EXPR has to be checked. Note that for index
   checking we cannot use the emit_range_check function (although very
   similar code needs to be generated in both cases) since for index
   checking the array type against which we are checking the indeces
   may be unconstrained and consequently we need to retrieve the
   actual index bounds from the array object itself
   (GNU_ARRAY_OBJECT). The place where we need to do that is in
   subprograms having unconstrained array formal parameters */

static tree
emit_index_check (tree gnu_array_object,
                  tree gnu_expr,
                  tree gnu_low,
                  tree gnu_high)
{
  tree gnu_expr_check;

  /* Checked expressions must be evaluated only once. */
  gnu_expr = protect_multiple_eval (gnu_expr);

  /* Must do this computation in the base type in case the expression's
     type is an unsigned subtypes.  */
  gnu_expr_check = convert (get_base_type (TREE_TYPE (gnu_expr)), gnu_expr);

  /* If GNU_LOW or GNU_HIGH are a PLACEHOLDER_EXPR, qualify them by
     the object we are handling. */
  gnu_low = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_low, gnu_array_object);
  gnu_high = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_high, gnu_array_object);

  /* There's no good type to use here, so we might as well use
     integer_type_node.   */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      build_binary_op (LT_EXPR, integer_type_node,
				       gnu_expr_check,
				       convert (TREE_TYPE (gnu_expr_check),
						gnu_low)),
		      build_binary_op (GT_EXPR, integer_type_node,
				       gnu_expr_check,
				       convert (TREE_TYPE (gnu_expr_check),
						gnu_high))),
     gnu_expr, CE_Index_Check_Failed);
}

/* GNU_COND contains the condition corresponding to an access, discriminant or
   range check of value GNU_EXPR.  Build a COND_EXPR that returns GNU_EXPR if
   GNU_COND is false and raises a CONSTRAINT_ERROR if GNU_COND is true.
   REASON is the code that says why the exception was raised.  */

static tree
emit_check (tree gnu_cond, tree gnu_expr, int reason)
{
  tree gnu_call;
  tree gnu_result;

  gnu_call = build_call_raise (reason, Empty);

  /* Use an outer COMPOUND_EXPR to make sure that GNU_EXPR will get evaluated
     in front of the comparison in case it ends up being a SAVE_EXPR.  Put the
     whole thing inside its own SAVE_EXPR so the inner SAVE_EXPR doesn't leak
     out.  */
  gnu_result = fold (build3 (COND_EXPR, TREE_TYPE (gnu_expr), gnu_cond,
			     build2 (COMPOUND_EXPR, TREE_TYPE (gnu_expr),
				     gnu_call, gnu_expr),
			     gnu_expr));

  /* If GNU_EXPR has side effects, make the outer COMPOUND_EXPR and
     protect it.  Otherwise, show GNU_RESULT has no side effects: we
     don't need to evaluate it just for the check.  */
  if (TREE_SIDE_EFFECTS (gnu_expr))
    gnu_result
      = build2 (COMPOUND_EXPR, TREE_TYPE (gnu_expr), gnu_expr, gnu_result);
  else
    TREE_SIDE_EFFECTS (gnu_result) = 0;

  /* ??? Unfortunately, if we don't put a SAVE_EXPR around this whole thing,
     we will repeatedly do the test.  It would be nice if GCC was able
     to optimize this and only do it once.  */
  return save_expr (gnu_result);
}

/* Return an expression that converts GNU_EXPR to GNAT_TYPE, doing
   overflow checks if OVERFLOW_P is nonzero and range checks if
   RANGE_P is nonzero.  GNAT_TYPE is known to be an integral type.
   If TRUNCATE_P is nonzero, do a float to integer conversion with
   truncation; otherwise round.  */

static tree
convert_with_check (Entity_Id gnat_type, tree gnu_expr, bool overflowp,
		    bool rangep, bool truncatep)
{
  tree gnu_type = get_unpadded_type (gnat_type);
  tree gnu_in_type = TREE_TYPE (gnu_expr);
  tree gnu_in_basetype = get_base_type (gnu_in_type);
  tree gnu_base_type = get_base_type (gnu_type);
  tree gnu_ada_base_type = get_ada_base_type (gnu_type);
  tree gnu_result = gnu_expr;

  /* If we are not doing any checks, the output is an integral type, and
     the input is not a floating type, just do the conversion.  This
     shortcut is required to avoid problems with packed array types
     and simplifies code in all cases anyway.   */
  if (!rangep && !overflowp && INTEGRAL_TYPE_P (gnu_base_type)
      && !FLOAT_TYPE_P (gnu_in_type))
    return convert (gnu_type, gnu_expr);

  /* First convert the expression to its base type.  This
     will never generate code, but makes the tests below much simpler.
     But don't do this if converting from an integer type to an unconstrained
     array type since then we need to get the bounds from the original
     (unpacked) type.  */
  if (TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE)
    gnu_result = convert (gnu_in_basetype, gnu_result);

  /* If overflow checks are requested,  we need to be sure the result will
     fit in the output base type.  But don't do this if the input
     is integer and the output floating-point.  */
  if (overflowp
      && !(FLOAT_TYPE_P (gnu_base_type) && INTEGRAL_TYPE_P (gnu_in_basetype)))
    {
      /* Ensure GNU_EXPR only gets evaluated once.  */
      tree gnu_input = protect_multiple_eval (gnu_result);
      tree gnu_cond = integer_zero_node;
      tree gnu_in_lb = TYPE_MIN_VALUE (gnu_in_basetype);
      tree gnu_in_ub = TYPE_MAX_VALUE (gnu_in_basetype);
      tree gnu_out_lb = TYPE_MIN_VALUE (gnu_base_type);
      tree gnu_out_ub = TYPE_MAX_VALUE (gnu_base_type);

      /* Convert the lower bounds to signed types, so we're sure we're
	 comparing them properly.  Likewise, convert the upper bounds
	 to unsigned types.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype) && TYPE_UNSIGNED (gnu_in_basetype))
	gnu_in_lb = convert (gnat_signed_type (gnu_in_basetype), gnu_in_lb);

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  && !TYPE_UNSIGNED (gnu_in_basetype))
	gnu_in_ub = convert (gnat_unsigned_type (gnu_in_basetype), gnu_in_ub);

      if (INTEGRAL_TYPE_P (gnu_base_type) && TYPE_UNSIGNED (gnu_base_type))
	gnu_out_lb = convert (gnat_signed_type (gnu_base_type), gnu_out_lb);

      if (INTEGRAL_TYPE_P (gnu_base_type) && !TYPE_UNSIGNED (gnu_base_type))
	gnu_out_ub = convert (gnat_unsigned_type (gnu_base_type), gnu_out_ub);

      /* Check each bound separately and only if the result bound
	 is tighter than the bound on the input type.  Note that all the
	 types are base types, so the bounds must be constant. Also,
	 the comparison is done in the base type of the input, which
	 always has the proper signedness.  First check for input
	 integer (which means output integer), output float (which means
	 both float), or mixed, in which case we always compare.
	 Note that we have to do the comparison which would *fail* in the
	 case of an error since if it's an FP comparison and one of the
	 values is a NaN or Inf, the comparison will fail.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_in_lb, gnu_out_lb)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? REAL_VALUES_LESS (TREE_REAL_CST (gnu_in_lb),
				 TREE_REAL_CST (gnu_out_lb))
	     : 1))
	gnu_cond
	  = invert_truthvalue
	    (build_binary_op (GE_EXPR, integer_type_node,
			      gnu_input, convert (gnu_in_basetype,
						  gnu_out_lb)));

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_out_ub, gnu_in_ub)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? REAL_VALUES_LESS (TREE_REAL_CST (gnu_out_ub),
				 TREE_REAL_CST (gnu_in_lb))
	     : 1))
	gnu_cond
	  = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node, gnu_cond,
			     invert_truthvalue
			     (build_binary_op (LE_EXPR, integer_type_node,
					       gnu_input,
					       convert (gnu_in_basetype,
							gnu_out_ub))));

      if (!integer_zerop (gnu_cond))
	gnu_result = emit_check (gnu_cond, gnu_input,
	                         CE_Overflow_Check_Failed);
    }

  /* Now convert to the result base type.  If this is a non-truncating
     float-to-integer conversion, round.  */
  if (INTEGRAL_TYPE_P (gnu_ada_base_type) && FLOAT_TYPE_P (gnu_in_basetype)
      && !truncatep)
    {
      REAL_VALUE_TYPE half_minus_pred_half, pred_half;
      tree gnu_conv, gnu_zero, gnu_comp, gnu_saved_result, calc_type;
      tree gnu_pred_half, gnu_add_pred_half, gnu_subtract_pred_half;
      const struct real_format *fmt;

      /* The following calculations depend on proper rounding to even
         of each arithmetic operation. In order to prevent excess
         precision from spoiling this property, use the widest hardware
         floating-point type.

         FIXME: For maximum efficiency, this should only be done for machines
         and types where intermediates may have extra precision.  */

      calc_type = longest_float_type_node;
      /* FIXME: Should not have padding in the first place */
      if (TREE_CODE (calc_type) == RECORD_TYPE
              && TYPE_IS_PADDING_P (calc_type))
        calc_type = TREE_TYPE (TYPE_FIELDS (calc_type));

      /* Compute the exact value calc_type'Pred (0.5) at compile time. */
      fmt = REAL_MODE_FORMAT (TYPE_MODE (calc_type));
      real_2expN (&half_minus_pred_half, -(fmt->p) - 1);
      REAL_ARITHMETIC (pred_half, MINUS_EXPR, dconsthalf,
                       half_minus_pred_half);
      gnu_pred_half = build_real (calc_type, pred_half);

      /* If the input is strictly negative, subtract this value
         and otherwise add it from the input. For 0.5, the result
         is exactly between 1.0 and the machine number preceding 1.0
         (for calc_type). Since the last bit of 1.0 is even, this 0.5
         will round to 1.0, while all other number with an absolute
         value less than 0.5 round to 0.0. For larger numbers exactly
         halfway between integers, rounding will always be correct as
         the true mathematical result will be closer to the higher
         integer compared to the lower one. So, this constant works
         for all floating-point numbers.

         The reason to use the same constant with subtract/add instead
         of a positive and negative constant is to allow the comparison
         to be scheduled in parallel with retrieval of the constant and
         conversion of the input to the calc_type (if necessary).
      */

      gnu_zero = convert (gnu_in_basetype, integer_zero_node);
      gnu_saved_result = save_expr (gnu_result);
      gnu_conv = convert (calc_type, gnu_saved_result);
      gnu_comp = build2 (GE_EXPR, integer_type_node,
	                gnu_saved_result, gnu_zero);
      gnu_add_pred_half
        = build2 (PLUS_EXPR, calc_type, gnu_conv, gnu_pred_half);
      gnu_subtract_pred_half
        = build2 (MINUS_EXPR, calc_type, gnu_conv, gnu_pred_half);
      gnu_result = build3 (COND_EXPR, calc_type, gnu_comp,
			   gnu_add_pred_half, gnu_subtract_pred_half);
    }

  if (TREE_CODE (gnu_ada_base_type) == INTEGER_TYPE
      && TYPE_HAS_ACTUAL_BOUNDS_P (gnu_ada_base_type)
      && TREE_CODE (gnu_result) == UNCONSTRAINED_ARRAY_REF)
    gnu_result = unchecked_convert (gnu_ada_base_type, gnu_result, false);
  else
    gnu_result = convert (gnu_ada_base_type, gnu_result);

  /* Finally, do the range check if requested.  Note that if the
     result type is a modular type, the range check is actually
     an overflow check.  */

  if (rangep
      || (TREE_CODE (gnu_base_type) == INTEGER_TYPE
	  && TYPE_MODULAR_P (gnu_base_type) && overflowp))
    gnu_result = emit_range_check (gnu_result, gnat_type);

  return convert (gnu_type, gnu_result);
}

/* Return 1 if GNU_EXPR can be directly addressed.  This is the case unless
   it is an expression involving computation or if it involves a reference
   to a bitfield or to a field not sufficiently aligned for its type.  */

static bool
addressable_p (tree gnu_expr)
{
  switch (TREE_CODE (gnu_expr))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
    case RESULT_DECL:
      /* All DECLs are addressable: if they are in a register, we can force
	 them to memory.  */
      return true;

    case UNCONSTRAINED_ARRAY_REF:
    case INDIRECT_REF:
    case CONSTRUCTOR:
    case NULL_EXPR:
    case SAVE_EXPR:
      return true;

    case COMPONENT_REF:
      return (!DECL_BIT_FIELD (TREE_OPERAND (gnu_expr, 1))
	      && (!STRICT_ALIGNMENT
	          /* If the field was marked as "semantically" addressable
		     in create_field_decl, we are guaranteed that it can
		     be directly addressed.  */
		  || !DECL_NONADDRESSABLE_P (TREE_OPERAND (gnu_expr, 1))
		  /* Otherwise it can nevertheless be directly addressed
		     if it has been sufficiently aligned in the record.  */
		  || DECL_ALIGN (TREE_OPERAND (gnu_expr, 1))
		       >= TYPE_ALIGN (TREE_TYPE (gnu_expr)))
	      && addressable_p (TREE_OPERAND (gnu_expr, 0)));

    case ARRAY_REF:  case ARRAY_RANGE_REF:
    case REALPART_EXPR:  case IMAGPART_EXPR:
    case NOP_EXPR:
      return addressable_p (TREE_OPERAND (gnu_expr, 0));

    case CONVERT_EXPR:
      return (AGGREGATE_TYPE_P (TREE_TYPE (gnu_expr))
	      && addressable_p (TREE_OPERAND (gnu_expr, 0)));

    case VIEW_CONVERT_EXPR:
      {
	/* This is addressable if we can avoid a copy.  */
	tree type = TREE_TYPE (gnu_expr);
	tree inner_type = TREE_TYPE (TREE_OPERAND (gnu_expr, 0));

	return (((TYPE_MODE (type) == TYPE_MODE (inner_type)
		  && (TYPE_ALIGN (type) <= TYPE_ALIGN (inner_type)
		      || TYPE_ALIGN (inner_type) >= BIGGEST_ALIGNMENT))
		 || ((TYPE_MODE (type) == BLKmode
		      || TYPE_MODE (inner_type) == BLKmode)
		     && (TYPE_ALIGN (type) <= TYPE_ALIGN (inner_type)
			 || TYPE_ALIGN (inner_type) >= BIGGEST_ALIGNMENT
			 || TYPE_ALIGN_OK (type)
			 || TYPE_ALIGN_OK (inner_type))))
		&& addressable_p (TREE_OPERAND (gnu_expr, 0)));
      }

    default:
      return false;
    }
}

/* Do the processing for the declaration of a GNAT_ENTITY, a type.  If
   a separate Freeze node exists, delay the bulk of the processing.  Otherwise
   make a GCC type for GNAT_ENTITY and set up the correspondence.  */

void
process_type (Entity_Id gnat_entity)
{
  tree gnu_old
    = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;
  tree gnu_new;

  /* If we are to delay elaboration of this type, just do any
     elaborations needed for expressions within the declaration and
     make a dummy type entry for this node and its Full_View (if
     any) in case something points to it.  Don't do this if it
     has already been done (the only way that can happen is if
     the private completion is also delayed).  */
  if (Present (Freeze_Node (gnat_entity))
      || (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity))
	  && Freeze_Node (Full_View (gnat_entity))
	  && !present_gnu_tree (Full_View (gnat_entity))))
    {
      elaborate_entity (gnat_entity);

      if (!gnu_old)
        {
	  tree gnu_decl = create_type_decl (get_entity_name (gnat_entity),
					    make_dummy_type (gnat_entity),
					    NULL, false, false, gnat_entity);

	  save_gnu_tree (gnat_entity, gnu_decl, false);
	  if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	      && Present (Full_View (gnat_entity)))
	    save_gnu_tree (Full_View (gnat_entity), gnu_decl, false);
	}

      return;
    }

  /* If we saved away a dummy type for this node it means that this
     made the type that corresponds to the full type of an incomplete
     type.  Clear that type for now and then update the type in the
     pointers.  */
  if (gnu_old)
    {
      if (TREE_CODE (gnu_old) != TYPE_DECL
	  || !TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old)))
	{
	  /* If this was a withed access type, this is not an error
	     and merely indicates we've already elaborated the type
	     already. */
	  gcc_assert (Is_Type (gnat_entity) && From_With_Type (gnat_entity));
	  return;
	}

      save_gnu_tree (gnat_entity, NULL_TREE, false);
    }

  /* Now fully elaborate the type.  */
  gnu_new = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 1);
  gcc_assert (TREE_CODE (gnu_new) == TYPE_DECL);

  /* If we have an old type and we've made pointers to this type,
     update those pointers.  */
  if (gnu_old)
    update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_old)),
		       TREE_TYPE (gnu_new));

  /* If this is a record type corresponding to a task or protected type
     that is a completion of an incomplete type, perform a similar update
     on the type.  */
  /* ??? Including protected types here is a guess. */

  if (IN (Ekind (gnat_entity), Record_Kind)
      && Is_Concurrent_Record_Type (gnat_entity)
      && present_gnu_tree (Corresponding_Concurrent_Type (gnat_entity)))
    {
      tree gnu_task_old
	= get_gnu_tree (Corresponding_Concurrent_Type (gnat_entity));

      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     NULL_TREE, false);
      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     gnu_new, false);

      update_pointer_to (TYPE_MAIN_VARIANT (TREE_TYPE (gnu_task_old)),
			 TREE_TYPE (gnu_new));
    }
}

/* GNAT_ASSOC is the front of the Component_Associations of an N_Aggregate.
   GNU_TYPE is the GCC type of the corresponding record.

   Return a CONSTRUCTOR to build the record.  */

static tree
assoc_to_constructor (Node_Id gnat_assoc, tree gnu_type)
{
  tree gnu_list, gnu_result;

  /* We test for GNU_FIELD being empty in the case where a variant
     was the last thing since we don't take things off GNAT_ASSOC in
     that case.  We check GNAT_ASSOC in case we have a variant, but it
     has no fields.  */

  for (gnu_list = NULL_TREE; Present (gnat_assoc);
       gnat_assoc = Next (gnat_assoc))
    {
      Node_Id gnat_field = First (Choices (gnat_assoc));
      tree gnu_field = gnat_to_gnu_field_decl (Entity (gnat_field));
      tree gnu_expr = gnat_to_gnu (Expression (gnat_assoc));

      /* The expander is supposed to put a single component selector name
	 in every record component association */
      gcc_assert (No (Next (gnat_field)));

      /* Ignore fields that have Corresponding_Discriminants since we'll
	 be setting that field in the parent.  */
      if (Present (Corresponding_Discriminant (Entity (gnat_field)))
	  && Is_Tagged_Type (Scope (Entity (gnat_field))))
	continue;

      /* Before assigning a value in an aggregate make sure range checks
	 are done if required.  Then convert to the type of the field.  */
      if (Do_Range_Check (Expression (gnat_assoc)))
	gnu_expr = emit_range_check (gnu_expr, Etype (gnat_field));

      gnu_expr = convert (TREE_TYPE (gnu_field), gnu_expr);

      /* Add the field and expression to the list.  */
      gnu_list = tree_cons (gnu_field, gnu_expr, gnu_list);
    }

  gnu_result = extract_values (gnu_list, gnu_type);

#ifdef ENABLE_CHECKING
  {
    tree gnu_field;

    /* Verify every enty in GNU_LIST was used.  */
    for (gnu_field = gnu_list; gnu_field; gnu_field = TREE_CHAIN (gnu_field))
      gcc_assert (TREE_ADDRESSABLE (gnu_field));
  }
#endif

  return gnu_result;
}

/* Builds a possibly nested constructor for array aggregates. GNAT_EXPR
   is the first element of an array aggregate. It may itself be an
   aggregate (an array or record aggregate). GNU_ARRAY_TYPE is the gnu type
   corresponding to the array aggregate. GNAT_COMPONENT_TYPE is the type
   of the array component. It is needed for range checking. */

static tree
pos_to_constructor (Node_Id gnat_expr, tree gnu_array_type,
                    Entity_Id gnat_component_type)
{
  tree gnu_expr_list = NULL_TREE;
  tree gnu_index = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_array_type));
  tree gnu_expr;

  for ( ; Present (gnat_expr); gnat_expr = Next (gnat_expr))
    {
      /* If the expression is itself an array aggregate then first build the
	 innermost constructor if it is part of our array (multi-dimensional
	 case).  */

      if (Nkind (gnat_expr) == N_Aggregate
	  && TREE_CODE (TREE_TYPE (gnu_array_type)) == ARRAY_TYPE
	  && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_array_type)))
	gnu_expr = pos_to_constructor (First (Expressions (gnat_expr)),
				       TREE_TYPE (gnu_array_type),
				       gnat_component_type);
      else
	{
	  gnu_expr = gnat_to_gnu (gnat_expr);

	  /* before assigning the element to the array make sure it is
	     in range */
	  if (Do_Range_Check (gnat_expr))
	    gnu_expr = emit_range_check (gnu_expr, gnat_component_type);
	}

      gnu_expr_list
	= tree_cons (gnu_index, convert (TREE_TYPE (gnu_array_type), gnu_expr),
		     gnu_expr_list);

      gnu_index = int_const_binop (PLUS_EXPR, gnu_index, integer_one_node, 0);
    }

  return gnat_build_constructor (gnu_array_type, nreverse (gnu_expr_list));
}

/* Subroutine of assoc_to_constructor: VALUES is a list of field associations,
   some of which are from RECORD_TYPE.  Return a CONSTRUCTOR consisting
   of the associations that are from RECORD_TYPE.  If we see an internal
   record, make a recursive call to fill it in as well.  */

static tree
extract_values (tree values, tree record_type)
{
  tree result = NULL_TREE;
  tree field, tem;

  for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
    {
      tree value = 0;

      /* _Parent is an internal field, but may have values in the aggregate,
	 so check for values first.  */
      if ((tem = purpose_member (field, values)))
	{
	  value = TREE_VALUE (tem);
	  TREE_ADDRESSABLE (tem) = 1;
	}

      else if (DECL_INTERNAL_P (field))
	{
	  value = extract_values (values, TREE_TYPE (field));
	  if (TREE_CODE (value) == CONSTRUCTOR
	      && VEC_empty (constructor_elt, CONSTRUCTOR_ELTS (value)))
	    value = 0;
	}
      else
	/* If we have a record subtype, the names will match, but not the
	   actual FIELD_DECLs.  */
	for (tem = values; tem; tem = TREE_CHAIN (tem))
	  if (DECL_NAME (TREE_PURPOSE (tem)) == DECL_NAME (field))
	    {
	      value = convert (TREE_TYPE (field), TREE_VALUE (tem));
	      TREE_ADDRESSABLE (tem) = 1;
	    }

      if (!value)
	continue;

      result = tree_cons (field, value, result);
    }

  return gnat_build_constructor (record_type, nreverse (result));
}

/* EXP is to be treated as an array or record.  Handle the cases when it is
   an access object and perform the required dereferences.  */

static tree
maybe_implicit_deref (tree exp)
{
  /* If the type is a pointer, dereference it.  */

  if (POINTER_TYPE_P (TREE_TYPE (exp)) || TYPE_FAT_POINTER_P (TREE_TYPE (exp)))
    exp = build_unary_op (INDIRECT_REF, NULL_TREE, exp);

  /* If we got a padded type, remove it too.  */
  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
      && TYPE_IS_PADDING_P (TREE_TYPE (exp)))
    exp = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (exp))), exp);

  return exp;
}

/* Protect EXP from multiple evaluation.  This may make a SAVE_EXPR.  */

tree
protect_multiple_eval (tree exp)
{
  tree type = TREE_TYPE (exp);

  /* If this has no side effects, we don't need to do anything.  */
  if (!TREE_SIDE_EFFECTS (exp))
    return exp;

  /* If it is a conversion, protect what's inside the conversion.
     Similarly, if we're indirectly referencing something, we only
     actually need to protect the address since the data itself can't
     change in these situations.  */
  else if (TREE_CODE (exp) == NON_LVALUE_EXPR
	   || TREE_CODE (exp) == NOP_EXPR || TREE_CODE (exp) == CONVERT_EXPR
	   || TREE_CODE (exp) == VIEW_CONVERT_EXPR
	   || TREE_CODE (exp) == INDIRECT_REF
	   || TREE_CODE (exp) == UNCONSTRAINED_ARRAY_REF)
    return build1 (TREE_CODE (exp), type,
		   protect_multiple_eval (TREE_OPERAND (exp, 0)));

  /* If EXP is a fat pointer or something that can be placed into a register,
     just make a SAVE_EXPR.  */
  if (TYPE_FAT_POINTER_P (type) || TYPE_MODE (type) != BLKmode)
    return save_expr (exp);

  /* Otherwise, dereference, protect the address, and re-reference.  */
  else
    return
      build_unary_op (INDIRECT_REF, type,
		      save_expr (build_unary_op (ADDR_EXPR,
						 build_reference_type (type),
						 exp)));
}

/* This is equivalent to stabilize_reference in GCC's tree.c, but we know how
   to handle our new nodes and we take extra arguments:

   FORCE says whether to force evaluation of everything,

   SUCCESS we set to true unless we walk through something we don't know how
   to stabilize, or through something which is not an lvalue and LVALUES_ONLY
   is true, in which cases we set to false.  */

tree
maybe_stabilize_reference (tree ref, bool force, bool lvalues_only,
			   bool *success)
{
  tree type = TREE_TYPE (ref);
  enum tree_code code = TREE_CODE (ref);
  tree result;

  /* Assume we'll success unless proven otherwise.  */
  *success = true;

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    case ADDR_EXPR:
      /*  A standalone ADDR_EXPR is never an lvalue, and this one can't
	  be nested inside an outer INDIRECT_REF, since INDIREC_REF goes
	  straight to stabilize_1.  */
      if (lvalues_only)
	goto failure;

      /* ... Fallthru ... */

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
    case VIEW_CONVERT_EXPR:
      result
	= build1 (code, type,
		  maybe_stabilize_reference (TREE_OPERAND (ref, 0), force,
					     lvalues_only, success));
      break;

    case INDIRECT_REF:
    case UNCONSTRAINED_ARRAY_REF:
      result = build1 (code, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 0),
						   force));
      break;

    case COMPONENT_REF:
     result = build3 (COMPONENT_REF, type,
		      maybe_stabilize_reference (TREE_OPERAND (ref, 0), force,
						 lvalues_only, success),
		      TREE_OPERAND (ref, 1), NULL_TREE);
      break;

    case BIT_FIELD_REF:
      result = build3 (BIT_FIELD_REF, type,
		       maybe_stabilize_reference (TREE_OPERAND (ref, 0), force,
						  lvalues_only, success),
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 1),
						   force),
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 2),
						   force));
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      result = build4 (code, type,
		       maybe_stabilize_reference (TREE_OPERAND (ref, 0), force,
						  lvalues_only, success),
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 1),
						   force),
		       NULL_TREE, NULL_TREE);
      break;

    case COMPOUND_EXPR:
      result = build2 (COMPOUND_EXPR, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 0),
						   force),
		       maybe_stabilize_reference (TREE_OPERAND (ref, 1), force,
						  lvalues_only, success));
      break;

    case ERROR_MARK:
      ref = error_mark_node;

      /* ...  Fallthru to failure ... */

      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
    failure:
      *success = false;
      return ref;
    }

  TREE_READONLY (result) = TREE_READONLY (ref);

  /* TREE_THIS_VOLATILE and TREE_SIDE_EFFECTS attached to the initial
     expression may not be sustained across some paths, such as the way via
     build1 for INDIRECT_REF.  We re-populate those flags here for the general
     case, which is consistent with the GCC version of this routine.

     Special care should be taken regarding TREE_SIDE_EFFECTS, because some
     paths introduce side effects where there was none initially (e.g. calls
     to save_expr), and we also want to keep track of that.  */

  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);
  TREE_SIDE_EFFECTS (result) |= TREE_SIDE_EFFECTS (ref);

  return result;
}

/* Wrapper around maybe_stabilize_reference, for common uses without
   lvalue restrictions and without need to examine the success
   indication.  */

tree
gnat_stabilize_reference (tree ref, bool force)
{
  bool stabilized;
  return maybe_stabilize_reference (ref, force, false, &stabilized);
}

/* Similar to stabilize_reference_1 in tree.c, but supports an extra
   arg to force a SAVE_EXPR for everything.  */

static tree
gnat_stabilize_reference_1 (tree e, bool force)
{
  enum tree_code code = TREE_CODE (e);
  tree type = TREE_TYPE (e);
  tree result;

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (TREE_CONSTANT (e) || code == SAVE_EXPR)
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_exceptional:
    case tcc_type:
    case tcc_declaration:
    case tcc_comparison:
    case tcc_statement:
    case tcc_expression:
    case tcc_reference:
      /* If this is a COMPONENT_REF of a fat pointer, save the entire
	 fat pointer.  This may be more efficient, but will also allow
	 us to more easily find the match for the PLACEHOLDER_EXPR.  */
      if (code == COMPONENT_REF
	  && TYPE_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (e, 0))))
	result = build3 (COMPONENT_REF, type,
			 gnat_stabilize_reference_1 (TREE_OPERAND (e, 0),
						     force),
			 TREE_OPERAND (e, 1), TREE_OPERAND (e, 2));
      else if (TREE_SIDE_EFFECTS (e) || force)
	return save_expr (e);
      else
	return e;
      break;

    case tcc_constant:
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;

    case tcc_binary:
      /* Recursively stabilize each operand.  */
      result = build2 (code, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (e, 0), force),
		       gnat_stabilize_reference_1 (TREE_OPERAND (e, 1),
						   force));
      break;

    case tcc_unary:
      /* Recursively stabilize each operand.  */
      result = build1 (code, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (e, 0),
						   force));
      break;

    default:
      gcc_unreachable ();
    }

  TREE_READONLY (result) = TREE_READONLY (e);

  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);
  TREE_SIDE_EFFECTS (result) |= TREE_SIDE_EFFECTS (e);
  return result;
}

/* Build a global constructor or destructor function.  METHOD_TYPE gives
   the type of the function and CDTORS points to the list of constructor
   or destructor functions to be invoked.  FIXME: Migrate into cgraph.  */

static void
build_global_cdtor (int method_type, tree *cdtors)
{
  tree body = 0;

  for (; *cdtors; *cdtors = TREE_CHAIN (*cdtors))
    {
      tree fn = TREE_VALUE (*cdtors);
      tree fntype = TREE_TYPE (fn);
      tree fnaddr = build1 (ADDR_EXPR, build_pointer_type (fntype), fn);
      tree fncall = build3 (CALL_EXPR, TREE_TYPE (fntype), fnaddr, NULL_TREE,
			    NULL_TREE);
      append_to_statement_list (fncall, &body);
    }

  cgraph_build_static_cdtor (method_type, body, DEFAULT_INIT_PRIORITY);
}

extern char *__gnat_to_canonical_file_spec (char *);

/* Convert Sloc into *LOCUS (a location_t).  Return true if this Sloc
   corresponds to a source code location and false if it doesn't.  In the
   latter case, we don't update *LOCUS.  We also set the Gigi global variable
   REF_FILENAME to the reference file name as given by sinput (i.e no
   directory).  */

bool
Sloc_to_locus (Source_Ptr Sloc, location_t *locus)
{
  /* If node not from source code, ignore.  */
  if (Sloc < 0)
    return false;

  /* Use the identifier table to make a hashed, permanent copy of the filename,
     since the name table gets reallocated after Gigi returns but before all
     the debugging information is output. The __gnat_to_canonical_file_spec
     call translates filenames from pragmas Source_Reference that contain host
     style syntax not understood by gdb. */
  locus->file
    = IDENTIFIER_POINTER
      (get_identifier
       (__gnat_to_canonical_file_spec
	(Get_Name_String (Full_Debug_Name (Get_Source_File_Index (Sloc))))));

  locus->line = Get_Logical_Line_Number (Sloc);

  ref_filename
    = IDENTIFIER_POINTER
      (get_identifier
       (Get_Name_String (Debug_Source_Name (Get_Source_File_Index (Sloc)))));;

  return true;
}

/* Similar to annotate_with_locus, but start with the Sloc of GNAT_NODE and
   don't do anything if it doesn't correspond to a source location.  */

static void
annotate_with_node (tree node, Node_Id gnat_node)
{
  location_t locus;

  if (!Sloc_to_locus (Sloc (gnat_node), &locus))
    return;

  annotate_with_locus (node, locus);
}

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   "&" substitution.  */

void
post_error (const char *msg, Node_Id node)
{
  String_Template temp;
  Fat_Pointer fp;

  temp.Low_Bound = 1, temp.High_Bound = strlen (msg);
  fp.Array = msg, fp.Bounds = &temp;
  if (Present (node))
    Error_Msg_N (fp, node);
}

/* Similar, but NODE is the node at which to post the error and ENT
   is the node to use for the "&" substitution.  */

void
post_error_ne (const char *msg, Node_Id node, Entity_Id ent)
{
  String_Template temp;
  Fat_Pointer fp;

  temp.Low_Bound = 1, temp.High_Bound = strlen (msg);
  fp.Array = msg, fp.Bounds = &temp;
  if (Present (node))
    Error_Msg_NE (fp, node, ent);
}

/* Similar, but NODE is the node at which to post the error, ENT is the node
   to use for the "&" substitution, and N is the number to use for the ^.  */

void
post_error_ne_num (const char *msg, Node_Id node, Entity_Id ent, int n)
{
  String_Template temp;
  Fat_Pointer fp;

  temp.Low_Bound = 1, temp.High_Bound = strlen (msg);
  fp.Array = msg, fp.Bounds = &temp;
  Error_Msg_Uint_1 = UI_From_Int (n);

  if (Present (node))
    Error_Msg_NE (fp, node, ent);
}

/* Similar to post_error_ne_num, but T is a GCC tree representing the
   number to write.  If the tree represents a constant that fits within
   a host integer, the text inside curly brackets in MSG will be output
   (presumably including a '^').  Otherwise that text will not be output
   and the text inside square brackets will be output instead.  */

void
post_error_ne_tree (const char *msg, Node_Id node, Entity_Id ent, tree t)
{
  char *newmsg = alloca (strlen (msg) + 1);
  String_Template temp = {1, 0};
  Fat_Pointer fp;
  char start_yes, end_yes, start_no, end_no;
  const char *p;
  char *q;

  fp.Array = newmsg, fp.Bounds = &temp;

  if (host_integerp (t, 1)
#if HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_INT
      &&
      compare_tree_int
      (t, (((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_INT - 1)) - 1)) < 0
#endif
      )
    {
      Error_Msg_Uint_1 = UI_From_Int (tree_low_cst (t, 1));
      start_yes = '{', end_yes = '}', start_no = '[', end_no = ']';
    }
  else
    start_yes = '[', end_yes = ']', start_no = '{', end_no = '}';

  for (p = msg, q = newmsg; *p; p++)
    {
      if (*p == start_yes)
	for (p++; *p != end_yes; p++)
	  *q++ = *p;
      else if (*p == start_no)
	for (p++; *p != end_no; p++)
	  ;
      else
	*q++ = *p;
    }

  *q = 0;

  temp.High_Bound = strlen (newmsg);
  if (Present (node))
    Error_Msg_NE (fp, node, ent);
}

/* Similar to post_error_ne_tree, except that NUM is a second
   integer to write in the message.  */

void
post_error_ne_tree_2 (const char *msg,
                      Node_Id node,
                      Entity_Id ent,
                      tree t,
                      int num)
{
  Error_Msg_Uint_2 = UI_From_Int (num);
  post_error_ne_tree (msg, node, ent, t);
}

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */

void
init_code_table (void)
{
  gnu_codes[N_And_Then] = TRUTH_ANDIF_EXPR;
  gnu_codes[N_Or_Else] = TRUTH_ORIF_EXPR;

  gnu_codes[N_Op_And] = TRUTH_AND_EXPR;
  gnu_codes[N_Op_Or] = TRUTH_OR_EXPR;
  gnu_codes[N_Op_Xor] = TRUTH_XOR_EXPR;
  gnu_codes[N_Op_Eq] = EQ_EXPR;
  gnu_codes[N_Op_Ne] = NE_EXPR;
  gnu_codes[N_Op_Lt] = LT_EXPR;
  gnu_codes[N_Op_Le] = LE_EXPR;
  gnu_codes[N_Op_Gt] = GT_EXPR;
  gnu_codes[N_Op_Ge] = GE_EXPR;
  gnu_codes[N_Op_Add] = PLUS_EXPR;
  gnu_codes[N_Op_Subtract] = MINUS_EXPR;
  gnu_codes[N_Op_Multiply] = MULT_EXPR;
  gnu_codes[N_Op_Mod] = FLOOR_MOD_EXPR;
  gnu_codes[N_Op_Rem] = TRUNC_MOD_EXPR;
  gnu_codes[N_Op_Minus] = NEGATE_EXPR;
  gnu_codes[N_Op_Abs] = ABS_EXPR;
  gnu_codes[N_Op_Not] = TRUTH_NOT_EXPR;
  gnu_codes[N_Op_Rotate_Left] = LROTATE_EXPR;
  gnu_codes[N_Op_Rotate_Right] = RROTATE_EXPR;
  gnu_codes[N_Op_Shift_Left] = LSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right] = RSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right_Arithmetic] = RSHIFT_EXPR;
}

#include "gt-ada-trans.h"
