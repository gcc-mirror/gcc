/* Process expressions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "real.h"
#include "rtl.h"
#include "flags.h"
#include "expr.h"
#include "java-tree.h"
#include "javaop.h"
#include "java-opcodes.h"
#include "jcf.h"
#include "java-except.h"
#include "parse.h"
#include "toplev.h"
#include "except.h"

static void flush_quick_stack PROTO ((void));
static void push_value PROTO ((tree));
static tree pop_value PROTO ((tree));
static void java_stack_swap PROTO ((void));
static void java_stack_dup PROTO ((int, int));
static tree build_java_athrow PROTO ((tree));
static void build_java_jsr PROTO ((tree, tree));
static void build_java_ret PROTO ((tree));
static void expand_java_multianewarray PROTO ((tree, int));
static void expand_java_arraystore PROTO ((tree));
static void expand_java_arrayload PROTO ((tree));
static void expand_java_array_length PROTO ((void));
static tree build_java_monitor PROTO ((tree, tree));
static void expand_java_pushc PROTO ((int, tree));
static void expand_java_return PROTO ((tree));
static void expand_java_NEW PROTO ((tree));
static void expand_java_INSTANCEOF PROTO ((tree));
static void expand_java_CHECKCAST PROTO ((tree));
static void expand_iinc PROTO ((unsigned int, int, int));
static void expand_java_binop PROTO ((tree, enum tree_code));
static void note_label PROTO ((int, int));
static void expand_compare PROTO ((enum tree_code, tree, tree, int));
static void expand_test PROTO ((enum tree_code, tree, int));
static void expand_cond PROTO ((enum tree_code, tree, int));
static void expand_java_goto PROTO ((int));
#if 0
static void expand_java_call PROTO ((int, int));
static void expand_java_ret PROTO ((tree)); 
#endif
static tree pop_arguments PROTO ((tree)); 
static void expand_invoke PROTO ((int, int, int)); 
static void expand_java_field_op PROTO ((int, int, int)); 
static void java_push_constant_from_pool PROTO ((struct JCF *, int)); 
 
static tree operand_type[59];
extern struct obstack permanent_obstack;

void
init_expr_processing()
{
  operand_type[21] = operand_type[54] = int_type_node;
  operand_type[22] = operand_type[55] = long_type_node;
  operand_type[23] = operand_type[56] = float_type_node;
  operand_type[24] = operand_type[57] = double_type_node;
  operand_type[25] = operand_type[58] = ptr_type_node;
}

/* We store the stack state in two places:
   Within a basic block, we use the quick_stack, which is a
   pushdown list (TREE_LISTs) of expression nodes.
   This is the top part of the stack;  below that we use find_stack_slot.
   At the end of a basic block, the quick_stack must be flushed
   to the stack slot array (as handled by find_stack_slot).
   Using quick_stack generates better code (especially when
   compiled without optimization), because we do not have to
   explicitly store and load trees to temporary variables.

   If a variable is on the quick stack, it means the value of variable
   when the quick stack was last flushed.  Conceptually, flush_quick_stack
   saves all the the quick_stack elements in parellel.  However, that is
   complicated, so it actually saves them (i.e. copies each stack value
   to is home virtual register) from low indexes.  This allows a quick_stack
   element at index i (counting from the bottom of stack the) to references
   slot virtuals for register that are >= i, but not those that are deeper.
   This convention makes most operations easier.  For example iadd works
   even when the stack contains (reg[0], reg[1]):  It results in the
   stack containing (reg[0]+reg[1]), which is OK.  However, some stack
   operations are more complicated.  For example dup given a stack
   containing (reg[0]) would yield (reg[0], reg[0]), which would violate
   the convention, since stack value 1 would refer to a register with
   lower index (reg[0]), which flush_quick_stack does not safely handle.
   So dup cannot just add an extra element to the quick_stack, but iadd can.
*/

tree quick_stack = NULL_TREE;

/* A free-list of unused permamnet TREE_LIST nodes. */
tree tree_list_free_list = NULL_TREE;

/* The stack pointer of the Java virtual machine.
   This does include the size of the quick_stack. */

int stack_pointer;

unsigned char *linenumber_table;
int linenumber_count;

tree
truthvalue_conversion (expr)
     tree expr;
{
  /* It is simpler and generates better code to have only TRUTH_*_EXPR
     or comparison expressions as truth values at this level.

     This function should normally be identity for Java.  */

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? boolean_false_node : boolean_true_node;

    case REAL_CST:
      return real_zerop (expr) ? boolean_false_node : boolean_true_node;

    /* are these legal? XXX JH */
    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is non-zero or zero.  */
      return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
                          truthvalue_conversion (TREE_OPERAND (expr, 1)),
                          truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
          >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
        return truthvalue_conversion (TREE_OPERAND (expr, 0));
      /* fall through to default */

    default:
      return fold (build (NE_EXPR, boolean_type_node, expr, boolean_false_node));
    }
}

#ifdef JAVA_USE_HANDLES
/* Given a pointer to a handle, get a pointer to an object. */

tree
unhand_expr (expr)
     tree expr;
{
  tree field, handle_type;
  expr = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (expr)), expr);
  handle_type = TREE_TYPE (expr);
  field = TYPE_FIELDS (handle_type);
  expr = build (COMPONENT_REF, TREE_TYPE (field), expr, field);
  return expr;
}
#endif

/* Save any stack slots that happen to be in the quick_stack into their
   home virtual register slots.

   The copy order is from low stack index to high, to support the invariant
   that the expression for a slot may contain decls for stack slots with
   higher (or the same) index, but not lower. */

static void
flush_quick_stack ()
{
  int stack_index = stack_pointer;
  register tree prev, cur, next;

  /* First reverse the quick_stack, and count the number of slots it has. */
  for (cur = quick_stack, prev = NULL_TREE; cur != NULL_TREE; cur = next)
    {
      next = TREE_CHAIN (cur);
      TREE_CHAIN (cur) = prev;
      prev = cur;
      stack_index -= 1 + TYPE_IS_WIDE (TREE_TYPE (TREE_VALUE (cur)));
    }
  quick_stack = prev;

  while (quick_stack != NULL_TREE)
    {
      tree decl;
      tree node = quick_stack, type;
      quick_stack = TREE_CHAIN (node);
      TREE_CHAIN (node) = tree_list_free_list;
      tree_list_free_list = node;
      node = TREE_VALUE (node);
      type = TREE_TYPE (node);

      decl = find_stack_slot (stack_index, type);
      if (decl != node)
	  expand_assignment (decl, node, 0, 0);
      stack_index += 1 + TYPE_IS_WIDE (type);
    }
}

void
push_type (type)
     tree type;
{
  int n_words;
  type = promote_type (type);
  n_words = 1 + TYPE_IS_WIDE (type);
  if (stack_pointer + n_words > DECL_MAX_STACK (current_function_decl))
    fatal ("stack overflow");
  stack_type_map[stack_pointer++] = type;
  n_words--;
  while (--n_words >= 0)
    stack_type_map[stack_pointer++] = TYPE_SECOND;
}

static void
push_value (value)
     tree value;
{
  tree type = TREE_TYPE (value);
  if (TYPE_PRECISION (type) < 32 && INTEGRAL_TYPE_P (type))
    {
      type = promote_type (type);
      value = convert (type, value);
    }
  push_type (type);
  if (tree_list_free_list == NULL_TREE)
    quick_stack = perm_tree_cons (NULL_TREE, value, quick_stack);
  else
    {
      tree node = tree_list_free_list;
      tree_list_free_list = TREE_CHAIN (tree_list_free_list);
      TREE_VALUE (node) = value;
      TREE_CHAIN (node) = quick_stack;
      quick_stack = node;
    }
}

/* Pop a type from the type stack.
   TYPE is the expected type.   Return the actual type, which must be
   convertible to TYPE, otherwise NULL_TREE is returned. */

tree
pop_type_0 (type)
     tree type;
{
  int n_words;
  tree t;
  if (TREE_CODE (type) == RECORD_TYPE)
    type = promote_type (type);
  n_words = 1 + TYPE_IS_WIDE (type);
  if (stack_pointer < n_words)
    fatal ("stack underflow");
  while (--n_words > 0)
    {
      if (stack_type_map[--stack_pointer] != void_type_node)
	fatal ("Invalid multi-word value on type stack");
    }
  t = stack_type_map[--stack_pointer];
  if (type == NULL_TREE || t == type)
    return t;
  if (INTEGRAL_TYPE_P (type) && INTEGRAL_TYPE_P (t)
      && TYPE_PRECISION (type) <= 32 && TYPE_PRECISION (t) <= 32)
      return t;
  if (TREE_CODE (type) == POINTER_TYPE && TREE_CODE (t) == POINTER_TYPE)
    {
      if (type == ptr_type_node || type == object_ptr_type_node)
	return t;
      else if (t == ptr_type_node)  /* Special case for null reference. */
	return type;
      else if (can_widen_reference_to (t, type))
	return t;
      /* This is a kludge, but matches what Sun's verifier does.
	 It can be tricked, but is safe as long as type errors
	 (i.e. interface method calls) are caught at run-time. */
      else if (CLASS_INTERFACE (TYPE_NAME (TREE_TYPE (type)))
	       && t == object_ptr_type_node)
	return t;
    }
  return NULL_TREE;
}

/* Pop a type from the type stack.
   TYPE is the expected type.  Return the actual type, which must be
   convertible to TYPE, otherwise call error. */

tree
pop_type (type)
     tree type;
{
  tree t = pop_type_0 (type);
  if (t != NULL_TREE)
    return t;
  error ("unexpected type on stack");
  return type;
}

/* Return 1f if SOURCE_TYPE can be safely widened to TARGET_TYPE.
   Handles array types and interfaces.  */

int
can_widen_reference_to (source_type, target_type)
     tree source_type, target_type;
{
  if (source_type == ptr_type_node || target_type == object_ptr_type_node)
    return 1;

  /* Get rid of pointers  */
  if (TREE_CODE (source_type) == POINTER_TYPE)
    source_type = TREE_TYPE (source_type);
  if (TREE_CODE (target_type) == POINTER_TYPE)
    target_type = TREE_TYPE (target_type);

  if (source_type == target_type)
    return 1;
  else
    {
      source_type = HANDLE_TO_CLASS_TYPE (source_type);
      target_type = HANDLE_TO_CLASS_TYPE (target_type);
      if (TYPE_ARRAY_P (source_type) || TYPE_ARRAY_P (target_type))
	{
	  HOST_WIDE_INT source_length, target_length;
	  if (TYPE_ARRAY_P (source_type) != TYPE_ARRAY_P (target_type))
	    return 0;
	  target_length = java_array_type_length (target_type);
	  if (target_length >= 0)
	    {
	      source_length = java_array_type_length (source_type);
	      if (source_length != target_length)
		return 0;
	    }
	  source_type = TYPE_ARRAY_ELEMENT (source_type);
	  target_type = TYPE_ARRAY_ELEMENT (target_type);
	  if (source_type == target_type)
	    return 1;
	  if (TREE_CODE (source_type) != POINTER_TYPE
	      || TREE_CODE (target_type) != POINTER_TYPE)
	    return 0;
	  return can_widen_reference_to (source_type, target_type);
	}
      else
	{
	  int source_depth = class_depth (source_type);
	  int target_depth = class_depth (target_type);

	  if (CLASS_INTERFACE (TYPE_NAME (target_type)))
	    {
	      /* target_type is OK if source_type or source_type ancestors
		 implement target_type. We handle multiple sub-interfaces  */

	      tree basetype_vec = TYPE_BINFO_BASETYPES (source_type);
	      int n = TREE_VEC_LENGTH (basetype_vec), i;
	      for (i=0 ; i < n; i++)
	        if (can_widen_reference_to 
		    (TREE_TYPE (TREE_VEC_ELT (basetype_vec, i)),
		     target_type))
		  return 1;
		if (n == 0)
		  return 0;
	    }

	  for ( ; source_depth > target_depth;  source_depth--) 
	    {
	      source_type = TYPE_BINFO_BASETYPE (source_type, 0); 
	    }
	  return source_type == target_type;
	}
    }
}

static tree
pop_value (type)
     tree type;
{
  type = pop_type (type);
  if (quick_stack)
    {
      tree node = quick_stack;
      quick_stack = TREE_CHAIN (quick_stack);
      TREE_CHAIN (node) = tree_list_free_list;
      tree_list_free_list = node;
      node = TREE_VALUE (node);
      return node;
    }
  else
    return find_stack_slot (stack_pointer, promote_type (type));
}


/* Pop and discrad the top COUNT stack slots. */

static void
java_stack_pop (count)
     int count;
{
  while (count > 0)
    {
      tree type, val;
      if (stack_pointer == 0)
	fatal ("stack underflow");
      type = stack_type_map[stack_pointer - 1];
      if (type == TYPE_SECOND)
	{
	  count--;
	  if (stack_pointer == 1 || count <= 0)
	    fatal ("stack underflow");
	  type = stack_type_map[stack_pointer - 2];
	}
      val = pop_value (type);
      count--;
    }
}

/* Implement the 'swap' operator (to swap two top stack slots). */

static void
java_stack_swap ()
{
  tree type1, type2;
  rtx temp;
  tree decl1, decl2;

  if (stack_pointer < 2
      || (type1 = stack_type_map[stack_pointer - 1]) == TYPE_UNKNOWN
      || (type2 = stack_type_map[stack_pointer - 2]) == TYPE_UNKNOWN
      || type1 == TYPE_SECOND || type2 == TYPE_SECOND
      || TYPE_IS_WIDE (type1) || TYPE_IS_WIDE (type2))
    fatal ("bad stack swap");

  flush_quick_stack ();
  decl1 = find_stack_slot (stack_pointer - 1, type1);
  decl2 = find_stack_slot (stack_pointer - 2, type2);
  temp = copy_to_reg (DECL_RTL (decl1));
  emit_move_insn (DECL_RTL (decl1), DECL_RTL (decl2));
  emit_move_insn (DECL_RTL (decl2), temp);
  stack_type_map[stack_pointer - 1] = type2;
  stack_type_map[stack_pointer - 2] = type1;
}

static void
java_stack_dup (size, offset)
     int size, offset;
{
  int low_index = stack_pointer - size - offset;
  int dst_index;
  if (low_index < 0)
    error ("stack underflow - dup* operation");

  flush_quick_stack ();

  stack_pointer += size;
  dst_index = stack_pointer;

  for (dst_index = stack_pointer;  --dst_index >= low_index; )
    {
      tree type;
      int src_index = dst_index - size;
      if (src_index < low_index)
	src_index = dst_index + size + offset;
      type = stack_type_map [src_index];
      if (type == TYPE_SECOND)
	{
	  if (src_index <= low_index)
	    fatal ("dup operation splits 64-bit number");
	  stack_type_map[dst_index] = type;
	  src_index--;  dst_index--;
	  type = stack_type_map[src_index];
	  if (! TYPE_IS_WIDE (type))
            fatal ("internal error - dup operation");
	}
      else if (TYPE_IS_WIDE (type))
	fatal ("internal error - dup operation");
      if (src_index != dst_index)
	{
	  tree src_decl = find_stack_slot (src_index, type);
	  tree dst_decl = find_stack_slot (dst_index, type);
	  emit_move_insn (DECL_RTL (dst_decl), DECL_RTL (src_decl));
	  stack_type_map[dst_index] = type;
	}
    }
}

/* Calls _Jv_Throw.  Discard the contents of the value stack. */

static tree
build_java_athrow (node)
    tree node;
{
  tree call;

  call = build (CALL_EXPR,
		void_type_node,
		build_address_of (throw_node),
		build_tree_list (NULL_TREE, node),
		NULL_TREE);
  TREE_SIDE_EFFECTS (call) = 1;
  expand_expr_stmt (call);
  java_stack_pop (stack_pointer);
}

/* Implementation for jsr/ret */

static void
build_java_jsr (where, ret)
    tree where;
    tree ret;
{
  tree ret_label = fold (build1 (ADDR_EXPR, return_address_type_node, ret));
  push_value (ret_label);
  flush_quick_stack ();
  expand_goto (where);
  expand_label (ret);
}

static void
build_java_ret (location)
  tree location;
{
  expand_computed_goto (location);
}
 
/* Implementation of operations on array: new, load, store, length */

/* Array core info access macros */

#define JAVA_ARRAY_LENGTH_OFFSET(A)					   \
  size_binop (CEIL_DIV_EXPR, 						   \
	      (DECL_FIELD_BITPOS					   \
		  (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (TREE_TYPE (A)))))), \
              size_int (BITS_PER_UNIT))

tree
decode_newarray_type (atype)
  int atype;
{
  switch (atype)
    {
    case 4:  return boolean_type_node;
    case 5:  return char_type_node;
    case 6:  return float_type_node;
    case 7:  return double_type_node;
    case 8:  return byte_type_node;
    case 9:  return short_type_node;
    case 10: return int_type_node;
    case 11: return long_type_node;
    default: return NULL_TREE;
    }
}

/* Map primitive type to the code used by OPCODE_newarray. */

int
encode_newarray_type (type)
     tree type;
{
  if (type == boolean_type_node)
    return 4;
  else if (type == char_type_node)
    return 5;
  else if (type == float_type_node)
    return 6;
  else if (type == double_type_node)
    return 7;
  else if (type == byte_type_node)
    return 8;
  else if (type == short_type_node)
    return 9;
  else if (type == int_type_node)
    return 10;
  else if (type == long_type_node)
    return 11;
  else
    fatal ("Can't compute type code - patch_newarray");
}

/* Build a call to _Jv_ThrowBadArrayIndex(), the
   ArrayIndexOfBoundsException exception handler.  */

static tree
build_java_throw_out_of_bounds_exception (index)
    tree index;
{
  tree node = build (CALL_EXPR, int_type_node,
		     build_address_of (soft_badarrayindex_node), 
		     build_tree_list (NULL_TREE, index), NULL_TREE);
  TREE_SIDE_EFFECTS (node) = 1;	/* Allows expansion within ANDIF */
  return (node);
}

/* Return the length of an array. Doesn't perform any checking on the nature
   or value of the array NODE. May be used to implement some bytecodes.  */

tree
build_java_array_length_access (node)
    tree node;
{
  tree type = TREE_TYPE (node);
  HOST_WIDE_INT length;
  if (!is_array_type_p (type))
    fatal ("array length on a non-array reference");
  length = java_array_type_length (type);
  if (length >= 0)
    return build_int_2 (length, 0);
  return fold (build1 (INDIRECT_REF,
		       int_type_node,
		       fold (build (PLUS_EXPR, ptr_type_node,
				    node, 
				    JAVA_ARRAY_LENGTH_OFFSET(node)))));
}

/* Optionally checks an array against the NULL pointer, eventually throwing a
   NullPointerException. It could replace signal handling, but tied to NULL.
   ARG1: the pointer to check, ARG2: the expression to use if
   the pointer is non-null and ARG3 the type that should be returned.   */

tree
build_java_arraynull_check (node, expr, type)
    tree node ATTRIBUTE_UNUSED;
    tree expr;
    tree type ATTRIBUTE_UNUSED;
{
#if 0
  static int java_array_access_throws_null_exception = 0;
  node = ???;
  if (java_array_access_throws_null_exception)
      return (build (COND_EXPR, 
		     type,
		     build (EQ_EXPR, int_type_node, node, null_pointer_node),
		     build_java_athrow (node), expr ));
  else
#endif
      return (expr);
}

static tree
java_array_data_offset (array)
     tree array;
{
  tree array_type = TREE_TYPE (TREE_TYPE (array));
  tree data_fld = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (array_type)));
  if (data_fld == NULL_TREE)
    return size_in_bytes (array_type);
  else
    return build_int_2 (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (data_fld))
			/ BITS_PER_UNIT, 0);
}

/* Implement array indexing (either as l-value or r-value).
   Returns a tree for ARRAY[INDEX], assume TYPE is the element type.
   Optionally performs bounds checking and/or test to NULL.
   At this point, ARRAY should have been verified as an array.  */

tree
build_java_arrayaccess (array, type, index)
    tree array, type, index;
{
  tree arith, node, throw = NULL_TREE;

  arith = fold (build (PLUS_EXPR, int_type_node,
		       java_array_data_offset (array),
		       fold (build (MULT_EXPR, int_type_node,
				    index, size_in_bytes(type)))));

  if (flag_bounds_check)
    {
      /* Generate:
       * (unsigned jint) INDEX >= (unsigned jint) LEN
       *    && throw ArrayIndexOutOfBoundsException.
       * Note this is equivalent to and more efficient than:
       * INDEX < 0 || INDEX >= LEN && throw ... */
      tree test;
      tree len = build_java_array_length_access (array);
      TREE_TYPE (len) = unsigned_int_type_node;
      test = fold (build (GE_EXPR, boolean_type_node, 
			       convert (unsigned_int_type_node, index),
			       len));
      if (! integer_zerop (test))
	{
	  throw = build (TRUTH_ANDIF_EXPR, int_type_node, test,
			 build_java_throw_out_of_bounds_exception (index));
	  /* allows expansion within COMPOUND */
	  TREE_SIDE_EFFECTS( throw ) = 1;
	}
    }

  node = build1 (INDIRECT_REF, type, 
		 fold (build (PLUS_EXPR, ptr_type_node, 
			      array, 
			      (throw ? build (COMPOUND_EXPR, int_type_node, 
					      throw, arith )
			             : arith))));

  return (fold (build_java_arraynull_check (array, node, type)));
}

/* Makes sure that INDEXED_TYPE is appropriate. If not, make it from
   ARRAY_NODE. This function is used to retrieve something less vague than
   a pointer type when indexing the first dimension of something like [[<t>.
   May return a corrected type, if necessary, otherwise INDEXED_TYPE is
   return unchanged.
   As a side effect, it also makes sure that ARRAY_NODE is an array.  */

static tree
build_java_check_indexed_type (array_node, indexed_type)
    tree array_node;
    tree indexed_type;
{
  tree elt_type;

  if (!is_array_type_p (TREE_TYPE (array_node)))
    fatal ("array indexing on a non-array reference");

  elt_type = (TYPE_ARRAY_ELEMENT (TREE_TYPE (TREE_TYPE (array_node))));

  if (indexed_type == ptr_type_node )
      return promote_type (elt_type);

  /* BYTE/BOOLEAN store and load are used for both type */
  if (indexed_type == byte_type_node && elt_type == boolean_type_node )
    return boolean_type_node;

  if (indexed_type != elt_type )
    fatal ("type array element mismatch");
  else
    return indexed_type;
}

/* newarray triggers a call to _Jv_NewArray. This function should be called
   with an integer code (the type of array to create) and get from the stack
   the size of the dimmension.  */

tree
build_newarray (atype_value, length)
     int atype_value;
     tree length;
{
  tree type = build_java_array_type (decode_newarray_type (atype_value),
				     TREE_CODE (length) == INTEGER_CST
				     ? TREE_INT_CST_LOW (length)
				     : -1);
  return build (CALL_EXPR, promote_type (type),
		build_address_of (soft_newarray_node),
		tree_cons (NULL_TREE, 
			   build_int_2 (atype_value, 0),
			   build_tree_list (NULL_TREE, length)),
		NULL_TREE);
}

/* Generates anewarray from a given CLASS_TYPE. Gets from the stack the size
   of the dimension. */

tree
build_anewarray (class_type, length)
    tree class_type;
    tree length;
{
  tree type = build_java_array_type (class_type,
				     TREE_CODE (length) == INTEGER_CST
				     ? TREE_INT_CST_LOW (length)
				     : -1);
  return build (CALL_EXPR, promote_type (type),
		build_address_of (soft_anewarray_node),
		tree_cons (NULL_TREE, length,
			   tree_cons (NULL_TREE, build_class_ref (class_type),
				      build_tree_list (NULL_TREE,
						       null_pointer_node))),
		NULL_TREE);
}

/* Return a node the evaluates 'new TYPE[LENGTH]'. */

tree
build_new_array (type, length)
     tree type;
     tree length;
{
  if (JPRIMITIVE_TYPE_P (type))
    return build_newarray (encode_newarray_type (type), length);
  else
    return build_anewarray (TREE_TYPE (type), length);
}

/* Generates a call to _Jv_NewMultiArray. multianewarray expects a
   class pointer, a number of dimensions and the matching number of
   dimensions. The argument list is NULL terminated.  */

static void
expand_java_multianewarray (class_type, ndim)
    tree class_type;
    int  ndim;
{
  int i;
  tree args = build_tree_list( NULL_TREE, null_pointer_node );

  for( i = 0; i < ndim; i++ )
    args = tree_cons (NULL_TREE, pop_value (int_type_node), args);

  push_value (build (CALL_EXPR,
		     promote_type (class_type),
		     build_address_of (soft_multianewarray_node),
		     tree_cons (NULL_TREE, build_class_ref (class_type),
				tree_cons (NULL_TREE, 
					   build_int_2 (ndim, 0), args )),
		     NULL_TREE));
}

/*  ARRAY[INDEX] <- RHS. build_java_check_indexed_type makes sure that
    ARRAY is an array type. May expand some bound checking and NULL
    pointer checking. RHS_TYPE_NODE we are going to store. In the case
    of the CHAR/BYTE/BOOLEAN SHORT, the type popped of the stack is an
    INT. In those cases, we make the convertion.

    if ARRAy is a reference type, the assignment is checked at run-time
    to make sure that the RHS can be assigned to the array element
    type. It is not necessary to generate this code if ARRAY is final.  */

static void
expand_java_arraystore (rhs_type_node)
     tree rhs_type_node;
{
  tree rhs_node    = pop_value ((INTEGRAL_TYPE_P (rhs_type_node) 
				 && TYPE_PRECISION (rhs_type_node) <= 32) ? 
				 int_type_node : rhs_type_node);
  tree index = pop_value (int_type_node);
  tree array = pop_value (ptr_type_node);

  rhs_type_node    = build_java_check_indexed_type (array, rhs_type_node);

  flush_quick_stack ();

  index = save_expr (index);
  array = save_expr (array);

  if (TREE_CODE (rhs_type_node) == POINTER_TYPE
      && !CLASS_FINAL (TYPE_NAME (TREE_TYPE (rhs_type_node))))
    {
      tree check = build (CALL_EXPR, void_type_node,
			  build_address_of (soft_checkarraystore_node),
			  tree_cons (NULL_TREE, array,
				     build_tree_list (NULL_TREE, rhs_node)),
			  NULL_TREE);
      TREE_SIDE_EFFECTS (check) = 1;
      expand_expr_stmt (check);
    }
  
  expand_assignment (build_java_arrayaccess (array,
					     rhs_type_node,
					     index),
		     rhs_node, 0, 0);
}

/* Expand the evaluation of ARRAY[INDEX]. build_java_check_indexed_type makes 
   sure that LHS is an array type. May expand some bound checking and NULL
   pointer checking.  
   LHS_TYPE_NODE is the type of ARRAY[INDEX]. But in the case of CHAR/BYTE/
   BOOLEAN/SHORT, we push a promoted type back to the stack.
*/

static void
expand_java_arrayload (lhs_type_node )
    tree lhs_type_node;
{
  tree load_node;
  tree index_node = pop_value (int_type_node);
  tree array_node = pop_value (ptr_type_node);

  index_node = save_expr (index_node);
  array_node = save_expr (array_node);
  lhs_type_node   = build_java_check_indexed_type (array_node, lhs_type_node);

  load_node = build_java_arrayaccess (array_node,
				      lhs_type_node,
				      index_node);

  if (INTEGRAL_TYPE_P (lhs_type_node) && TYPE_PRECISION (lhs_type_node) <= 32)
    load_node = fold (build1 (NOP_EXPR, int_type_node, load_node));
  push_value (load_node);
}

/* Expands .length. Makes sure that we deal with and array and may expand
   a NULL check on the array object.  */

static void
expand_java_array_length ()
{
  tree array  = pop_value (ptr_type_node);
  tree length = build_java_array_length_access (array);

  push_value (build_java_arraynull_check (array, length, int_type_node));
}

/* Emit code for the call to _Jv_Monitor{Enter,Exit}. CALL can be
   either soft_monitorenter_node or soft_monitorexit_node.  */

static tree
build_java_monitor (call, object)
    tree call;
    tree object;
{
  return (build (CALL_EXPR,
		 void_type_node,
		 build_address_of (call),
		 build_tree_list (NULL_TREE, object),
		 NULL_TREE));
}

/* Emit code for one of the PUSHC instructions. */

static void
expand_java_pushc (ival, type)
     int ival;
     tree type;
{
  tree value;
  if (type == ptr_type_node && ival == 0)
    value = null_pointer_node;
  else if (type == int_type_node || type == long_type_node)
    {
      value = build_int_2 (ival, ival < 0 ? -1 : 0);
      TREE_TYPE (value) = type;
    }
  else if (type == float_type_node || type == double_type_node)
    {
      REAL_VALUE_TYPE x;
#ifdef REAL_ARITHMETIC
      REAL_VALUE_FROM_INT (x, ival, 0, TYPE_MODE (type));
#else
      x = ival;
#endif
      value = build_real (type, x);
    }
  else
    fatal ("internal error in expand_java_pushc");
  push_value (value);
}

static void
expand_java_return (type)
     tree type;
{
  if (type == void_type_node)
    expand_null_return ();
  else
    {
      tree retval = pop_value (type);
      tree res = DECL_RESULT (current_function_decl);
      retval = build (MODIFY_EXPR, TREE_TYPE (res), res, retval);
      TREE_SIDE_EFFECTS (retval) = 1;
      expand_return (retval);
    }
}

tree
build_address_of (value)
     tree value;
{
  return build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (value)), value);
}

static void
expand_java_NEW (type)
     tree type;
{
  if (! CLASS_LOADED_P (type))
    load_class (type, 1);
  layout_class_methods (type);
  push_value (build (CALL_EXPR, promote_type (type),
		     build_address_of (alloc_object_node),
		     tree_cons (NULL_TREE, build_class_ref (type),
				build_tree_list (NULL_TREE,
						 size_in_bytes (type))),
		     NULL_TREE));
}

static void
expand_java_INSTANCEOF (type)
     tree type;
{
  tree value = pop_value (object_ptr_type_node);
  value = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (soft_instanceof_node)),
		 build_address_of (soft_instanceof_node),
		 tree_cons (NULL_TREE, value,
			    build_tree_list (NULL_TREE,
					     build_class_ref (type))),
		 NULL_TREE);
  push_value (value);
}

static void
expand_java_CHECKCAST (type)
     tree type;
{
  tree value = pop_value (ptr_type_node);
  value = build (CALL_EXPR, promote_type (type),
		 build_address_of (soft_checkcast_node),
		 tree_cons (NULL_TREE, build_class_ref (type),
			    build_tree_list (NULL_TREE, value)),
		 NULL_TREE);
  push_value (value);
}

static void
expand_iinc (local_var_index, ival, pc)
     unsigned int local_var_index;
     int ival;
     int pc;
{
    tree local_var, res;
    tree constant_value;

    flush_quick_stack ();
    local_var = find_local_variable (local_var_index, int_type_node, pc);
    constant_value = build_int_2 (ival, ival < 0 ? -1 : 0);
    res = fold (build (PLUS_EXPR, int_type_node, local_var, constant_value));
    expand_assignment (local_var, res, 0, 0);
}

tree
build_java_binop (op, type, arg1, arg2)
     enum tree_code op;
     tree type, arg1, arg2;
{
  tree mask;
  switch (op)
    {
    case URSHIFT_EXPR:
      {
	tree u_type = unsigned_type (type);
	arg1 = convert (u_type, arg1);
	arg1 = build_java_binop (RSHIFT_EXPR, u_type, arg1, arg2);
	return convert (type, arg1);
      }
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      mask = build_int_2 (TYPE_PRECISION (TREE_TYPE (arg1)) - 1, 0);
      arg2 = fold (build (BIT_AND_EXPR, int_type_node, arg2, mask));
      break;

    case COMPARE_L_EXPR:  /* arg1 > arg2 ?  1 : arg1 == arg2 ? 0 : -1 */
    case COMPARE_G_EXPR:  /* arg1 < arg2 ? -1 : arg1 == arg2 ? 0 :  1 */
      arg1 = save_expr (arg1);  arg2 = save_expr (arg2);
      {
	tree ifexp1 = fold ( build (op == COMPARE_L_EXPR ? GT_EXPR : LT_EXPR,
				    boolean_type_node, arg1, arg2));
	tree ifexp2 = fold ( build (EQ_EXPR, boolean_type_node, arg1, arg2));
	tree second_compare = fold (build (COND_EXPR, int_type_node,
					   ifexp2, integer_zero_node,
					   op == COMPARE_L_EXPR
					   ? integer_negative_one_node
					   : integer_one_node));
	return fold (build (COND_EXPR, int_type_node, ifexp1,
			    op == COMPARE_L_EXPR ? integer_one_node
			    : integer_negative_one_node,
			    second_compare));
      }
    case COMPARE_EXPR:
      arg1 = save_expr (arg1);  arg2 = save_expr (arg2);
      {
	tree ifexp1 = fold ( build (LT_EXPR, boolean_type_node, arg1, arg2));
	tree ifexp2 = fold ( build (GT_EXPR, boolean_type_node, arg1, arg2));
	tree second_compare = fold ( build (COND_EXPR, int_type_node,
					    ifexp2, integer_one_node,
					    integer_zero_node));
	return fold (build (COND_EXPR, int_type_node,
			    ifexp1, integer_negative_one_node, second_compare));
      }

    case TRUNC_MOD_EXPR:
      if (TREE_CODE (type) == REAL_TYPE)
	{
	  tree call;
	  if (type != double_type_node)
	    {
	      arg1 = convert (double_type_node, arg1);
	      arg2 = convert (double_type_node, arg2);
	    }
	  call = build (CALL_EXPR, double_type_node,
			build_address_of (soft_fmod_node),
			tree_cons (NULL_TREE, arg1,
				   build_tree_list (NULL_TREE, arg2)),
			NULL_TREE);
	  if (type != double_type_node)
	    call = convert (type, call);
	  return call;
	}
      break;
    default:  ;
    }
  return fold (build (op, type, arg1, arg2));
}

static void
expand_java_binop (type, op)
     tree type;  enum tree_code op;
{
  tree larg, rarg;
  tree ltype = type;
  tree rtype = type;
  switch (op)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case URSHIFT_EXPR:
      rtype = int_type_node;
      rarg = pop_value (rtype);
      break;
    default:
      rarg = pop_value (rtype);
    }
  larg = pop_value (ltype);
  push_value (build_java_binop (op, type, larg, rarg));
}

/* Lookup the field named NAME in *TYPEP or its super classes.
   If not found, return NULL_TREE.
   (If the *TYPEP is not found, return error_mark_node.)
   If found, return the FIELD_DECL, and set *TYPEP to the
   class containing the field. */

tree
lookup_field (typep, name)
     tree *typep;
     tree name;
{
  if (CLASS_P (*typep) && !CLASS_LOADED_P (*typep))
    {
      load_class (*typep, 1);
      if (!TYPE_SIZE (*typep) || TREE_CODE (TYPE_SIZE (*typep)) == ERROR_MARK)
	return error_mark_node;
    }
  do
    {
      tree field, basetype_vec;
      int n, i;

      for (field = TYPE_FIELDS (*typep); field; field = TREE_CHAIN (field))
	if (DECL_NAME (field) == name)
	  return field;

      /* Process implemented interfaces. */
      basetype_vec = TYPE_BINFO_BASETYPES (*typep);
      n = TREE_VEC_LENGTH (basetype_vec);
      for (i = 0; i < n; i++)
	{
	  tree t = BINFO_TYPE (TREE_VEC_ELT (basetype_vec, i));
	  if ((field = lookup_field (&t, name)))
	    return field;
	}
      *typep = CLASSTYPE_SUPER (*typep);
    } while (*typep);
  return NULL_TREE;
}

/* Look up the field named NAME in object SELF_VALUE,
   which has class SELF_CLASS (a non-handle RECORD_TYPE).
   SELF_VALUE is NULL_TREE if looking for a static field. */

tree
build_field_ref (self_value, self_class, name)
     tree self_value, self_class, name;
{
  tree base_class = self_class;
  tree field_decl = lookup_field (&base_class, name);
  if (field_decl == NULL_TREE)
    {
      error ("field `%s' not found", IDENTIFIER_POINTER (name));
      return error_mark_node;
    }
  if (self_value == NULL_TREE)
    {
      return build_static_field_ref (field_decl);
    }
  else
    {
      tree base_handle_type = promote_type (base_class);
      if (base_handle_type != TREE_TYPE (self_value))
	self_value = fold (build1 (NOP_EXPR, base_handle_type, self_value));
#ifdef JAVA_USE_HANDLES
      self_value = unhand_expr (self_value);
#endif
      self_value = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (self_value)),
			   self_value);
      return fold (build (COMPONENT_REF, TREE_TYPE (field_decl),
			  self_value, field_decl));
    }
}

tree
lookup_label (pc)
     int pc;
{
  tree name;
  char buf[32];
  ASM_GENERATE_INTERNAL_LABEL(buf, "LJpc=", pc);
  name = get_identifier (buf);
  if (IDENTIFIER_LOCAL_VALUE (name))
    return IDENTIFIER_LOCAL_VALUE (name);
  else
    {
      /* The type of the address of a label is return_address_type_node. */
      tree decl = create_label_decl (name);
      LABEL_PC (decl) = pc;
      label_rtx (decl);
      return pushdecl (decl);
    }
}

/* Generate a unique name for the purpose of loops and switches
   labels, and try-catch-finally blocks label or temporary variables.  */

tree
generate_name ()
{
  static int l_number = 0;
  char buff [32];
  ASM_GENERATE_INTERNAL_LABEL(buff, "LJv", l_number);
  l_number++;
  return get_identifier (buff);
}

tree
create_label_decl (name)
     tree name;
{
  tree decl;
  push_obstacks (&permanent_obstack, &permanent_obstack);
  decl = build_decl (LABEL_DECL, name, 
		     TREE_TYPE (return_address_type_node));
  pop_obstacks ();
  DECL_CONTEXT (decl) = current_function_decl;
  DECL_IGNORED_P (decl) = 1;
  return decl;
}

/* This maps a bytecode offset (PC) to various flags. */
char *instruction_bits;

static void
note_label (current_pc, target_pc)
     int current_pc ATTRIBUTE_UNUSED, target_pc;
{
  lookup_label (target_pc);
  instruction_bits [target_pc] |= BCODE_JUMP_TARGET;
}

/* Emit code to jump to TARGET_PC if VALUE1 CONDITION VALUE2,
   where CONDITION is one of one the compare operators. */

static void
expand_compare (condition, value1, value2, target_pc)
     enum tree_code condition;
     tree value1, value2;
     int target_pc;
{
  tree target = lookup_label (target_pc);
  tree cond = fold (build (condition, boolean_type_node, value1, value2));
  expand_start_cond (truthvalue_conversion (cond), 0);
  expand_goto (target);
  expand_end_cond ();
}

/* Emit code for a TEST-type opcode. */

static void
expand_test (condition, type, target_pc)
     enum tree_code condition;
     tree type;
     int target_pc;
{
  tree value1, value2;
  flush_quick_stack ();
  value1 = pop_value (type);
  value2 = (type == ptr_type_node) ? null_pointer_node : integer_zero_node;
  expand_compare (condition, value1, value2, target_pc);
}

/* Emit code for a COND-type opcode. */

static void
expand_cond (condition, type, target_pc)
     enum tree_code condition;
     tree type;
     int target_pc;
{
  tree value1, value2;
  flush_quick_stack ();
  /* note: pop values in opposite order */
  value2 = pop_value (type);
  value1 = pop_value (type);
  /* Maybe should check value1 and value2 for type compatibility ??? */
  expand_compare (condition, value1, value2, target_pc);
}

static void
expand_java_goto (target_pc)
     int target_pc;
{
  tree target_label = lookup_label (target_pc);
  flush_quick_stack ();
  expand_goto (target_label);
}

#if 0
static void
expand_java_call (target_pc, return_address)
     int target_pc, return_address;
{
  tree target_label = lookup_label (target_pc);
  tree value = build_int_2 (return_address, return_address < 0 ? -1 : 0);
  push_value (value);
  flush_quick_stack ();
  expand_goto (target_label);
}

static void
expand_java_ret (return_address)
     tree return_address ATTRIBUTE_UNUSED;
{
  warning ("ret instruction not implemented");
#if 0
  tree target_label = lookup_label (target_pc);
  flush_quick_stack ();
  expand_goto (target_label);
#endif
}
#endif

/* Recursive helper function to pop argument types during verifiation. */

void
pop_argument_types (arg_types)
     tree arg_types;
{
  if (arg_types == end_params_node)
    return;
  if (TREE_CODE (arg_types) == TREE_LIST)
    {
      pop_argument_types (TREE_CHAIN (arg_types));
      pop_type (TREE_VALUE (arg_types));
      return;
    }
  abort ();
}

static tree
pop_arguments (arg_types)
     tree arg_types;
{
  if (arg_types == end_params_node)
    return NULL_TREE;
  if (TREE_CODE (arg_types) == TREE_LIST)
    {
      tree tail = pop_arguments (TREE_CHAIN (arg_types));
      tree type = TREE_VALUE (arg_types);
      tree arg = pop_value (type);
#ifdef PROMOTE_PROTOTYPES
      if (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)
	  && INTEGRAL_TYPE_P (type))
	arg = convert (integer_type_node, arg);
#endif
      return tree_cons (NULL_TREE, arg, tail);
    }
  abort ();
}

/* Build an expression to initialize the class CLAS.
   if EXPR is non-NULL, returns an expression to first call the initializer
   (if it is needed) and then calls EXPR. */

tree
build_class_init (clas, expr)
     tree clas, expr;
{
  tree init;
  if (inherits_from_p (current_class, clas))
    return expr;
  init = build (CALL_EXPR, void_type_node,
		build_address_of (soft_initclass_node),
		build_tree_list (NULL_TREE, build_class_ref (clas)),
		NULL_TREE);
  TREE_SIDE_EFFECTS (init) = 1;
  if (expr != NULL_TREE)
    {
      expr = build (COMPOUND_EXPR, TREE_TYPE (expr), init, expr);
      TREE_SIDE_EFFECTS (expr) = 1;
      return expr;
    }
  return init;
}

static tree methods_ident = NULL_TREE;
static tree ncode_ident = NULL_TREE;
tree dtable_ident = NULL_TREE;

tree
build_known_method_ref (method, method_type, self_type, method_signature, arg_list)
     tree method, method_type ATTRIBUTE_UNUSED, self_type,
          method_signature ATTRIBUTE_UNUSED, arg_list ATTRIBUTE_UNUSED;
{
  tree func;
  if (is_compiled_class (self_type))
    {
      make_decl_rtl (method, NULL, 1);
      func = build1 (ADDR_EXPR, method_ptr_type_node, method);
    }
  else
    {
      /* We don't know whether the method has been (statically) compiled.
	 Compile this code to get a reference to the method's code:
	 
	 SELF_TYPE->methods[METHOD_INDEX].ncode
	 
	 This is guaranteed to work (assuming SELF_TYPE has
	 been initialized), since if the method is not compiled yet,
	 its ncode points to a trampoline that forces compilation. */
      
      int method_index = 0;
      tree meth;
      tree ref = build_class_ref (self_type);
      ref = build1 (INDIRECT_REF, class_type_node, ref);
      if (ncode_ident == NULL_TREE)
	ncode_ident = get_identifier ("ncode");
      if (methods_ident == NULL_TREE)
	methods_ident = get_identifier ("methods");
      ref = build (COMPONENT_REF, method_ptr_type_node, ref,
		   lookup_field (&class_type_node, methods_ident));
      for (meth = TYPE_METHODS (CLASS_TO_HANDLE_TYPE (self_type));
	   ; meth = TREE_CHAIN (meth))
	{
	  if (method == meth)
	    break;
	  if (meth == NULL_TREE)
	    fatal ("method '%s' not found in class",
		   IDENTIFIER_POINTER (DECL_NAME (method)));
	  method_index++;
	}
      method_index *= int_size_in_bytes (method_type_node);
      ref = fold (build (PLUS_EXPR, method_ptr_type_node,
			 ref, build_int_2 (method_index, 0)));
      ref = build1 (INDIRECT_REF, method_type_node, ref);
      func = build (COMPONENT_REF, nativecode_ptr_type_node,
		    ref,
		    lookup_field (&method_type_node, ncode_ident));
    }
  return func;
}

tree
invoke_build_dtable (is_invoke_interface, arg_list)
     int is_invoke_interface;
     tree arg_list;
{
  tree dtable, objectref;

  TREE_VALUE (arg_list) = save_expr (TREE_VALUE (arg_list));

  /* If we're dealing with interfaces and if the objectref
     argument is an array then get the dispatch table of the class
     Object rather than the one from the objectref.  */
  objectref = (is_invoke_interface 
	       && is_array_type_p (TREE_TYPE (TREE_VALUE (arg_list))) ?
	       object_type_node : TREE_VALUE (arg_list));
  
  if (dtable_ident == NULL_TREE)
    dtable_ident = get_identifier ("vtable");
  dtable = build1 (INDIRECT_REF, object_type_node, objectref );
  dtable = build (COMPONENT_REF, dtable_ptr_type, dtable,
		  lookup_field (&object_type_node, dtable_ident));

  return dtable;
}

tree 
build_invokevirtual (dtable, method)
     tree dtable, method;
{
  tree func;
  tree nativecode_ptr_ptr_type_node
    = build_pointer_type (nativecode_ptr_type_node);
  int method_index = TREE_INT_CST_LOW (DECL_VINDEX (method));
  /* Add one to skip "class" field of dtable, and one to skip unused
     vtable entry (for C++ compatibility). */
  method_index += 2;
  method_index
    *= int_size_in_bytes (nativecode_ptr_ptr_type_node);
  func = fold (build (PLUS_EXPR, nativecode_ptr_ptr_type_node,
		      dtable, build_int_2 (method_index, 0)));
  func = build1 (INDIRECT_REF, nativecode_ptr_type_node, func);

  return func;
}

tree
build_invokeinterface (dtable, method_name, method_signature)
     tree dtable, method_name, method_signature;
{
  static tree class_ident = NULL_TREE;
  tree lookup_arg;

  /* We expand invokeinterface here.  _Jv_LookupInterfaceMethod() will
     ensure that the selected method exists, is public and not
     abstract nor static.  */
	    
  if (class_ident == NULL_TREE)
    class_ident = get_identifier ("class");
  
  dtable = build1 (INDIRECT_REF, dtable_type, dtable);
  dtable = build (COMPONENT_REF, class_ptr_type, dtable,
		  lookup_field (&dtable_type, class_ident));
  lookup_arg = build_tree_list (NULL_TREE, 
				(build_utf8_ref 
				 (unmangle_classname
				  (IDENTIFIER_POINTER(method_signature),
				   IDENTIFIER_LENGTH(method_signature)))));
  lookup_arg = tree_cons (NULL_TREE, dtable,
			  tree_cons (NULL_TREE, build_utf8_ref (method_name),
				     lookup_arg));
  return build (CALL_EXPR, ptr_type_node, 
		build_address_of (soft_lookupinterfacemethod_node),
		lookup_arg, NULL_TREE);
}
  
/* Expand one of the invoke_* opcodes.
   OCPODE is the specific opcode.
   METHOD_REF_INDEX is an index into the constant pool.
   NARGS is the number of arguments, or -1 if not specified. */

static void
expand_invoke (opcode, method_ref_index, nargs)
     int opcode;
     int method_ref_index;
     int nargs ATTRIBUTE_UNUSED;
{
  tree method_signature = COMPONENT_REF_SIGNATURE(&current_jcf->cpool, method_ref_index);
  tree method_name = COMPONENT_REF_NAME (&current_jcf->cpool, method_ref_index);
  tree self_type = get_class_constant
    (current_jcf, COMPONENT_REF_CLASS_INDEX(&current_jcf->cpool, method_ref_index));
  char *self_name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (self_type)));
  tree call, func, method, arg_list, method_type;

  if (! CLASS_LOADED_P (self_type))
    {
      load_class (self_type, 1);
      if (TREE_CODE (TYPE_SIZE (self_type)) == ERROR_MARK)
	fatal ("failed to find class '%s'", self_name);
    }
  layout_class_methods (self_type);

  if (method_name == init_identifier_node)
    method = lookup_java_constructor (CLASS_TO_HANDLE_TYPE (self_type),
				      method_signature);
  else
    method = lookup_java_method (CLASS_TO_HANDLE_TYPE (self_type),
				 method_name, method_signature);
  if (method == NULL_TREE)
    {
      error ("Class '%s' has no method named '%s' matching signature '%s'",
	     self_name,
	     IDENTIFIER_POINTER (method_name),
	     IDENTIFIER_POINTER (method_signature));
    }
  /* Invoke static can't invoke static/abstract method */
  else if (opcode == OPCODE_invokestatic)
    {
      if (!METHOD_STATIC (method))
	{
	  error ("invokestatic on non static method");
	  method = NULL_TREE;
	}
      else if (METHOD_ABSTRACT (method))
	{
	  error ("invokestatic on abstract method");
	  method = NULL_TREE;
	}
    }
  else
    {
      if (METHOD_STATIC (method))
	{
	  error ("invoke[non-static] on static method");
	  method = NULL_TREE;
	}
    }

  if (method == NULL_TREE)
    {
      method_type = get_type_from_signature (method_signature);
      pop_arguments (TYPE_ARG_TYPES (method_type));
      if (opcode != OPCODE_invokestatic) 
	pop_type (self_type);
      method_type = promote_type (TREE_TYPE (method_type));
      push_value (convert (method_type, integer_zero_node));
      return;
    }

  method_type = TREE_TYPE (method);
  arg_list = pop_arguments (TYPE_ARG_TYPES (method_type));
  flush_quick_stack ();

  func = NULL_TREE;
  if (opcode == OPCODE_invokestatic || opcode == OPCODE_invokespecial
      || (opcode == OPCODE_invokevirtual
	  && (METHOD_PRIVATE (method)
	      || METHOD_FINAL (method) 
	      || CLASS_FINAL (TYPE_NAME (self_type)))))
    func = build_known_method_ref (method, method_type, self_type,
				   method_signature, arg_list);
  else
    {
      tree dtable = invoke_build_dtable (opcode == OPCODE_invokeinterface, 
					 arg_list);
      if (opcode == OPCODE_invokevirtual)
	func = build_invokevirtual (dtable, method);
      else
	func = build_invokeinterface (dtable, method_name, method_signature);
    }
  func = build1 (NOP_EXPR, build_pointer_type (method_type), func);
  call = build (CALL_EXPR, TREE_TYPE (method_type), func, arg_list, NULL_TREE);
  TREE_SIDE_EFFECTS (call) = 1;

  if (TREE_CODE (TREE_TYPE (method_type)) == VOID_TYPE)
    expand_expr_stmt (call);
  else
    {
      push_value (call);
      flush_quick_stack ();
    }
}


/* Expand an operation to extract from or store into a field.
   IS_STATIC is 1 iff the field is static.
   IS_PUTTING is 1 for putting into a field;  0 for getting from the field.
   FIELD_REF_INDEX is an index into the constant pool.  */

static void
expand_java_field_op (is_static, is_putting, field_ref_index)
     int is_static;
     int is_putting;
     int field_ref_index;
{
  tree self_type = 
      get_class_constant (current_jcf, 
			  COMPONENT_REF_CLASS_INDEX (&current_jcf->cpool, 
						     field_ref_index));
  char *self_name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (self_type)));
  tree field_name = COMPONENT_REF_NAME (&current_jcf->cpool, field_ref_index);
  tree field_signature = COMPONENT_REF_SIGNATURE (&current_jcf->cpool, 
						  field_ref_index);
  tree field_type = get_type_from_signature (field_signature);
  tree new_value = is_putting ? pop_value (field_type) : NULL_TREE;
  tree field_ref;
  int is_error = 0;
  tree field_decl = lookup_field (&self_type, field_name);
  if (field_decl == error_mark_node)
    {
      is_error = 1;
    }
  else if (field_decl == NULL_TREE)
    {
      error ("Missing field '%s' in '%s'",
	     IDENTIFIER_POINTER (field_name), self_name);
      is_error = 1;
    }
  else if (build_java_signature (TREE_TYPE (field_decl)) != field_signature)
    {
      error ("Mismatching signature for field '%s' in '%s'",
	     IDENTIFIER_POINTER (field_name), self_name);
      is_error = 1;
    }
  field_ref = is_static ? NULL_TREE : pop_value (self_type);
  if (is_error)
    {
      if (! is_putting)
	push_value (convert (field_type, integer_zero_node));
      flush_quick_stack ();
      return;
    }

  /* Inline references to java.lang.PRIMTYPE.TYPE.
     In addition to being a useful (minor) optimization,
     this is also needed to avoid circularities in the implementation
     of these fields in libjava. */
  if (field_name == TYPE_identifier_node && ! is_putting
      && ! flag_emit_class_files && field_type == class_ptr_type
      && strncmp (self_name, "java.lang.", 10) == 0)
    {
      tree typ = build_primtype_type_ref (self_name);
      if (typ)
	{
	  push_value (typ);
	  return;
	}
    }

  field_ref = build_field_ref (field_ref, self_type, field_name);
  if (is_static)
    field_ref = build_class_init (self_type, field_ref);
  if (is_putting)
    {
      flush_quick_stack ();
      if (FIELD_FINAL (field_decl))
	{
	  if (DECL_CONTEXT (field_decl) != current_class)
	    error_with_decl (field_decl,
		     "assignment to final field `%s' not in field's class");
	  else if (FIELD_STATIC (field_decl))
	    {
	      if (!IS_CLINIT (current_function_decl))
		error_with_decl (field_decl, 
             "assignment to final static field `%s' not in class initializer");
	    }
	  else
	    {
	      if (! DECL_CONSTRUCTOR_P (current_function_decl))
		error_with_decl (field_decl, "assignment to final field `%s' "
				 "not in constructor");
	    }
	}
      expand_assignment (field_ref, new_value, 0, 0);
    }
  else
    push_value (field_ref);
}

tree
build_primtype_type_ref (self_name)
    char *self_name;
{
  char *class_name = self_name+10;
  tree typ;
  if (strncmp(class_name, "Byte", 4) == 0)
    typ = byte_type_node;
  else if (strncmp(class_name, "Short", 5) == 0)
    typ = short_type_node;
  else if (strncmp(class_name, "Integer", 7) == 0)
    typ = int_type_node;
  else if (strncmp(class_name, "Long", 4) == 0)
    typ = long_type_node;
  else if (strncmp(class_name, "Float", 5) == 0)
    typ = float_type_node;
  else if (strncmp(class_name, "Double", 6) == 0)
    typ = double_type_node;
  else if (strncmp(class_name, "Boolean", 7) == 0)
    typ = boolean_type_node;
  else if (strncmp(class_name, "Char", 4) == 0)
    typ = char_type_node;
  else if (strncmp(class_name, "Void", 4) == 0)
    typ = void_type_node;
  else
    typ = NULL_TREE;
  if (typ != NULL_TREE)
    return build_class_ref (typ);
  else
    return NULL_TREE;
}

void
load_type_state (label)
     tree label;
{
  int i;
  tree vec = LABEL_TYPE_STATE (label);
  int cur_length = TREE_VEC_LENGTH (vec);
  stack_pointer = cur_length - DECL_MAX_LOCALS(current_function_decl);
  for (i = 0; i < cur_length; i++)
    type_map [i] = TREE_VEC_ELT (vec, i);
}

/* Do the expansion of a Java switch. With Gcc, switches are front-end
   dependant things, but they rely on gcc routines. This function is
   placed here because it uses things defined locally in parse.y. */

static tree
case_identity (t, v)
     tree t __attribute__ ((__unused__));
     tree v;
{
  return v;
}

struct rtx_def *
java_lang_expand_expr (exp, target, tmode, modifier)
     register tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree current;

  switch (TREE_CODE (exp))
    {
    case NEW_ARRAY_INIT:
      {
	rtx tmp;
	tree array_type = TREE_TYPE (TREE_TYPE (exp));
	tree element_type = TYPE_ARRAY_ELEMENT (array_type);
	tree data_fld = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (array_type)));
	HOST_WIDE_INT ilength = java_array_type_length (array_type);
	tree length = build_int_2 (ilength, 0);
	tree init = TREE_OPERAND (exp, 0);
	tree array_decl;
#if 0
	/* Enable this once we can set the vtable field statically.  FIXME */
	if (TREE_CONSTANT (init) && TREE_STATIC (exp)
	    && JPRIMITIVE_TYPE_P (element_type))
	  {
	    tree temp, value, init_decl;
	    START_RECORD_CONSTRUCTOR (temp, object_type_node);
	    PUSH_FIELD_VALUE (temp, "vtable",
			      null_pointer_node /* FIXME */
			      );
	    PUSH_FIELD_VALUE (temp, "sync_info", null_pointer_node);
	    FINISH_RECORD_CONSTRUCTOR (temp);
	    START_RECORD_CONSTRUCTOR (value, array_type);
	    PUSH_SUPER_VALUE (value, temp);
	    PUSH_FIELD_VALUE (value, "length", length);
	    PUSH_FIELD_VALUE (value, "data", init);
	    FINISH_RECORD_CONSTRUCTOR (value);

	    init_decl = build_decl (VAR_DECL, generate_name (), array_type);
	    pushdecl_top_level (init_decl);
	    TREE_STATIC (init_decl) = 1;
	    DECL_INITIAL (init_decl) = value;
	    DECL_IGNORED_P (init_decl) = 1;
	    TREE_READONLY (init_decl) = 1;
	    make_decl_rtl (init_decl, NULL, 1);
	    init = build1 (ADDR_EXPR, TREE_TYPE (exp), init_decl);
	    return expand_expr (init, target, tmode, modifier);
	  }
#endif
	array_decl = build_decl (VAR_DECL, NULL_TREE, TREE_TYPE (exp));
	expand_decl (array_decl);
	tmp = expand_assignment (array_decl,
				 build_new_array (element_type, length),
				 1, 0);
	if (TREE_CONSTANT (init)
	    && ilength >= 10 && JPRIMITIVE_TYPE_P (element_type))
	  {
	    tree init_decl = build_decl (VAR_DECL, generate_name (),
					 TREE_TYPE (init));
	    pushdecl_top_level (init_decl);
	    TREE_STATIC (init_decl) = 1;
	    DECL_INITIAL (init_decl) = init;
	    DECL_IGNORED_P (init_decl) = 1;
	    TREE_READONLY (init_decl) = 1;
	    make_decl_rtl (init_decl, NULL, 1);
	    init = init_decl;
	  }
	expand_assignment (build (COMPONENT_REF, TREE_TYPE (data_fld),
				  build1 (INDIRECT_REF, array_type, array_decl),
				  data_fld),
			   init, 0, 0);
	return tmp;
      }
    case BLOCK:
      if (BLOCK_EXPR_BODY (exp))
	{
	  tree local;
	  tree body = BLOCK_EXPR_BODY (exp);
	  struct rtx_def *to_return;
	  pushlevel (2);	/* 2 and above */
	  expand_start_bindings (0);
	  local = BLOCK_EXPR_DECLS (exp);
	  while (local)
	    {
	      tree next = TREE_CHAIN (local);
	      layout_decl (local, 0);
	      expand_decl (pushdecl (local));
	      local = next;
	    }
	  /* Avoid deep recursion for long block.  */
	  while (TREE_CODE (body) == COMPOUND_EXPR)
	    {
	      expand_expr (TREE_OPERAND (body, 0), const0_rtx, VOIDmode, 0);
	      emit_queue ();
	      body = TREE_OPERAND (body, 1);
	    }
	  to_return = expand_expr (body, target, tmode, modifier);
	  poplevel (1, 1, 0);
	  expand_end_bindings (getdecls (), 1, 0);
	  return to_return;
	}
      break;

    case CASE_EXPR:
      {
	tree duplicate;
	if (pushcase (TREE_OPERAND (exp, 0), case_identity,
		      build_decl (LABEL_DECL, NULL_TREE, NULL_TREE), 
		      &duplicate) == 2)
	  {
	    EXPR_WFL_LINECOL (wfl_operator) = EXPR_WFL_LINECOL (exp);
	    parse_error_context
	      (wfl_operator, "Duplicate case label: `%s'",
	       print_int_node (TREE_OPERAND (exp, 0)));
	  }
	return const0_rtx;
      }

    case DEFAULT_EXPR:
      pushcase (NULL_TREE, 0, 
		build_decl (LABEL_DECL, NULL_TREE, NULL_TREE), NULL);
      return const0_rtx;

    case SWITCH_EXPR:
      expand_start_case (0, TREE_OPERAND (exp, 0), int_type_node, "switch");
      expand_expr_stmt (TREE_OPERAND (exp, 1));
      expand_end_case (TREE_OPERAND (exp, 0));
      return const0_rtx;

    case TRY_EXPR:
      /* We expand a try[-catch] block */

      /* Expand the try block */
      expand_eh_region_start ();
      expand_expr_stmt (TREE_OPERAND (exp, 0));
      expand_start_all_catch ();

      /* Expand all catch clauses (EH handlers) */
      for (current = TREE_OPERAND (exp, 1); current; 
	   current = TREE_CHAIN (current))
	{
	  tree type;
	  tree catch = TREE_OPERAND (current, 0);
	  tree decl = BLOCK_EXPR_DECLS (catch);
	  type = (decl ? TREE_TYPE (TREE_TYPE (decl)) : NULL_TREE);
	  start_catch_handler (prepare_eh_table_type (type));
	  expand_expr_stmt (TREE_OPERAND (current, 0));

	  expand_resume_after_catch ();
	  end_catch_handler ();
	}
      expand_end_all_catch ();
      break;

    default:
      fatal ("Can't expand '%s' tree - java_lang_expand_expr",
	     tree_code_name [TREE_CODE (exp)]);
    }
}

void
expand_byte_code (jcf, method)
     JCF *jcf;
     tree method;
{
  int PC;
  int i;
  int saw_index;
  unsigned char *linenumber_pointer;
  int dead_code_index = -1;

#undef RET /* Defined by config/i386/i386.h */
#undef AND /* Causes problems with opcodes for iand and land. */
#undef PTR
#define BCODE byte_ops
#define BYTE_type_node byte_type_node
#define SHORT_type_node short_type_node
#define INT_type_node int_type_node
#define LONG_type_node long_type_node
#define CHAR_type_node char_type_node
#define PTR_type_node ptr_type_node
#define FLOAT_type_node float_type_node
#define DOUBLE_type_node double_type_node
#define VOID_type_node void_type_node
  jint INT_temp;
  unsigned char* byte_ops;
  long length = DECL_CODE_LENGTH (method);

  stack_pointer = 0;
  JCF_SEEK (jcf, DECL_CODE_OFFSET (method));
  byte_ops = jcf->read_ptr;

#define CONST_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define CONST_INDEX_2 (saw_index = 1, IMMEDIATE_u2)
#define VAR_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define VAR_INDEX_2 (saw_index = 1, IMMEDIATE_u2)

#define CHECK_PC_IN_RANGE(PC) ((void)1) /* Already handled by verifier. */

  instruction_bits = oballoc (length + 1);
  bzero (instruction_bits, length + 1);

  /* We make an initial pass of the line number table, to note
     which instructions have associated line number entries. */
  linenumber_pointer = linenumber_table;
  for (i = 0; i < linenumber_count; i++)
    {
      int pc = GET_u2 (linenumber_pointer);
      linenumber_pointer += 4;
      if (pc >= length)
	warning ("invalid PC in line number table");
      else
	{
	  if ((instruction_bits[pc] & BCODE_HAS_LINENUMBER) != 0)
	    instruction_bits[pc] |= BCODE_HAS_MULTI_LINENUMBERS;
	  instruction_bits[pc] |= BCODE_HAS_LINENUMBER;
	}
    }  

  /* Do a preliminary pass.
   * This figures out which PC can be the targets of jumps. */
  for (PC = 0; PC < length;)
    {
      int oldpc = PC; /* PC at instruction start. */
      instruction_bits [PC] |=  BCODE_INSTRUCTION_START;
      switch (byte_ops[PC++])
	{
#define JAVAOP(OPNAME, OPCODE, OPKIND, OPERAND_TYPE, OPERAND_VALUE) \
        case OPCODE: \
	  PRE_##OPKIND(OPERAND_TYPE, OPERAND_VALUE); \
	  break;

#define NOTE_LABEL(PC) note_label(oldpc, PC)

#define PRE_PUSHC(OPERAND_TYPE, OPERAND_VALUE) (void)(OPERAND_VALUE);
#define PRE_LOAD(OPERAND_TYPE, OPERAND_VALUE) (void)(OPERAND_VALUE);
#define PRE_STORE(OPERAND_TYPE, OPERAND_VALUE) (void)(OPERAND_VALUE);
#define PRE_STACK(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define PRE_UNOP(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define PRE_BINOP(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define PRE_CONVERT(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define PRE_CONVERT2(OPERAND_TYPE, OPERAND_VALUE) /* nothing */

#define PRE_SPECIAL(OPERAND_TYPE, INSTRUCTION) \
  PRE_SPECIAL_##INSTRUCTION(OPERAND_TYPE)
#define PRE_SPECIAL_IINC(OPERAND_TYPE) \
  ((void) IMMEDIATE_u1, (void) IMMEDIATE_s1)
#define PRE_SPECIAL_ENTER(IGNORE) /* nothing */
#define PRE_SPECIAL_EXIT(IGNORE) /* nothing */
#define PRE_SPECIAL_THROW(IGNORE) /* nothing */
#define PRE_SPECIAL_BREAK(IGNORE) /* nothing */

/* two forms of wide instructions */
#define PRE_SPECIAL_WIDE(IGNORE) \
  { \
    int modified_opcode = IMMEDIATE_u1; \
    if (modified_opcode == OPCODE_iinc)	\
      { \
	(void) IMMEDIATE_u2;	/* indexbyte1 and indexbyte2 */ \
	(void) IMMEDIATE_s2;	/* constbyte1 and constbyte2 */ \
      } \
    else \
      { \
	(void) IMMEDIATE_u2;	/* indexbyte1 and indexbyte2 */ \
      } \
  }

/* nothing */ /* XXX JH */

#define PRE_IMPL(IGNORE1, IGNORE2) /* nothing */

#define PRE_MONITOR(OPERAND_TYPE, OPERAND_VALUE) /* nothing */

#define PRE_RETURN(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define PRE_ARRAY(OPERAND_TYPE, SUBOP) \
	  PRE_ARRAY_##SUBOP(OPERAND_TYPE)
#define PRE_ARRAY_LOAD(TYPE) /* nothing */
#define PRE_ARRAY_STORE(TYPE) /* nothing */
#define PRE_ARRAY_LENGTH(TYPE) /* nothing */
#define PRE_ARRAY_NEW(TYPE) PRE_ARRAY_NEW_##TYPE
#define PRE_ARRAY_NEW_NUM ((void) IMMEDIATE_u1)
#define PRE_ARRAY_NEW_PTR ((void) IMMEDIATE_u2)
#define PRE_ARRAY_NEW_MULTI ((void) IMMEDIATE_u2, (void) IMMEDIATE_u1)

#define PRE_TEST(OPERAND_TYPE, OPERAND_VALUE) NOTE_LABEL (oldpc+IMMEDIATE_s2)
#define PRE_COND(OPERAND_TYPE, OPERAND_VALUE) NOTE_LABEL (oldpc+IMMEDIATE_s2)
#define PRE_BRANCH(OPERAND_TYPE, OPERAND_VALUE) \
  saw_index = 0;  INT_temp = (OPERAND_VALUE); \
  if (!saw_index)  NOTE_LABEL(oldpc + INT_temp);
#define PRE_JSR(OPERAND_TYPE, OPERAND_VALUE) \
  saw_index = 0;  INT_temp = (OPERAND_VALUE); \
  if (!saw_index)  NOTE_LABEL(oldpc + INT_temp);

#define PRE_RET(OPERAND_TYPE, OPERAND_VALUE)  (void)(OPERAND_VALUE)

#define PRE_SWITCH(OPERAND_TYPE, TABLE_OR_LOOKUP) \
  PC = (PC + 3) / 4 * 4; PRE_##TABLE_OR_LOOKUP##_SWITCH

#define PRE_LOOKUP_SWITCH						\
  { jint default_offset = IMMEDIATE_s4;  jint npairs = IMMEDIATE_s4;	\
    NOTE_LABEL (default_offset+oldpc);					\
    if (npairs >= 0)							\
      while (--npairs >= 0) {						\
       jint match ATTRIBUTE_UNUSED = IMMEDIATE_s4;			\
       jint offset = IMMEDIATE_s4;					\
       NOTE_LABEL (offset+oldpc); }					\
  }

#define PRE_TABLE_SWITCH				\
  { jint default_offset = IMMEDIATE_s4;			\
    jint low = IMMEDIATE_s4; jint high = IMMEDIATE_s4;	\
    NOTE_LABEL (default_offset+oldpc);			\
    if (low <= high)					\
     while (low++ <= high) {				\
       jint offset = IMMEDIATE_s4;			\
       NOTE_LABEL (offset+oldpc); }			\
  }

#define PRE_FIELD(MAYBE_STATIC, PUT_OR_GET) (void)(IMMEDIATE_u2);
#define PRE_OBJECT(MAYBE_STATIC, PUT_OR_GET) (void)(IMMEDIATE_u2);
#define PRE_INVOKE(MAYBE_STATIC, IS_INTERFACE) \
  (void)(IMMEDIATE_u2); \
  PC += 2 * IS_INTERFACE /* for invokeinterface */;

#include "javaop.def"
#undef JAVAOP
	}
    } /* for */

  if (! verify_jvm_instructions (jcf, byte_ops, length))
    return;

  /* Translate bytecodes to rtl instructions. */
  linenumber_pointer = linenumber_table;
  for (PC = 0; PC < length;)
    {
      if ((instruction_bits [PC] & BCODE_TARGET) != 0 || PC == 0)
	{
	  tree label = lookup_label (PC);
          flush_quick_stack ();
	  if ((instruction_bits [PC] & BCODE_TARGET) != 0)
	    expand_label (label);
	  if (LABEL_VERIFIED (label) || PC == 0)
	    load_type_state (label);
	}

      if (! (instruction_bits [PC] & BCODE_VERIFIED))
	{
	  if (dead_code_index == -1)
	    {
	      /* This is the start of a region of unreachable bytecodes.
                 They still need to be processed in order for EH ranges
                 to get handled correctly.  However, we can simply
                 replace these bytecodes with nops.  */
	      dead_code_index = PC;
            }
          
          /* Turn this bytecode into a nop.  */
          byte_ops[PC] = 0x0;
        }
       else
        {
	  if (dead_code_index != -1)
	    {
              /* We've just reached the end of a region of dead code.  */
              warning ("Unreachable bytecode from %d to before %d.",
                       dead_code_index, PC);
              dead_code_index = -1;
            }
	}

      /* Handle possible line number entry for this PC.

	 This code handles out-of-order and multiple linenumbers per PC,
	 but is optimized for the case of line numbers increasing
	 monotonically with PC. */
      if ((instruction_bits[PC] & BCODE_HAS_LINENUMBER) != 0)
	{
	  if ((instruction_bits[PC] & BCODE_HAS_MULTI_LINENUMBERS) != 0
	      || GET_u2 (linenumber_pointer) != PC)
	    linenumber_pointer = linenumber_table;
	  while (linenumber_pointer < linenumber_table + linenumber_count * 4)
	    {
	      int pc = GET_u2 (linenumber_pointer);
	      linenumber_pointer += 4;
	      if (pc == PC)
		{
		  lineno = GET_u2 (linenumber_pointer - 2);
		  emit_line_note (input_filename, lineno);
		  if (!(instruction_bits[PC] & BCODE_HAS_MULTI_LINENUMBERS))
		    break;
		}
	    }
	}
      maybe_start_try (PC);
      maybe_pushlevels (PC);

      PC = process_jvm_instruction (PC, byte_ops, length);

      maybe_poplevels (PC);
      maybe_end_try (PC);
    } /* for */
  
  if (dead_code_index != -1)
    {
      /* We've just reached the end of a region of dead code.  */
      warning ("Unreachable bytecode from %d to the end of the method.", 
              dead_code_index);
    }
}

static void
java_push_constant_from_pool (jcf, index)
     JCF *jcf;
     int index;
{
  tree c;
  if (JPOOL_TAG (jcf, index) == CONSTANT_String)
    {
      tree name;
      push_obstacks (&permanent_obstack, &permanent_obstack);
      name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index));
      index = alloc_name_constant (CONSTANT_String, name);
      c = build_ref_from_constant_pool (index);
      TREE_TYPE (c) = promote_type (string_type_node);
      pop_obstacks ();
    }
  else
    c = get_constant (jcf, index);
  push_value (c);
} 

int
process_jvm_instruction (PC, byte_ops, length)
     int PC;
     unsigned char* byte_ops;
     long length ATTRIBUTE_UNUSED;
{ 
  const char *opname; /* Temporary ??? */
  int oldpc = PC; /* PC at instruction start. */

  /* If the instruction is at the beginning of a exception handler,
     replace the top of the stack with the thrown object reference */
  if (instruction_bits [PC] & BCODE_EXCEPTION_TARGET)
    {
      tree type = pop_type (ptr_type_node);
      push_value (build1 (NOP_EXPR, type, soft_exceptioninfo_call_node));
    }

  switch (byte_ops[PC++])
    {
#define JAVAOP(OPNAME, OPCODE, OPKIND, OPERAND_TYPE, OPERAND_VALUE) \
    case OPCODE: \
      opname = #OPNAME; \
      OPKIND(OPERAND_TYPE, OPERAND_VALUE); \
      break;

#define RET(OPERAND_TYPE, OPERAND_VALUE) 				\
  {									\
    int saw_index = 0;							\
    int index     = OPERAND_VALUE;					\
    build_java_ret (find_local_variable (index, ptr_type_node, oldpc));	\
  }

#define JSR(OPERAND_TYPE, OPERAND_VALUE)		\
  {							\
    tree where = lookup_label (oldpc+OPERAND_VALUE);	\
    tree ret   = lookup_label (PC);			\
    build_java_jsr (where, ret);			\
    load_type_state (ret);				\
  }

/* Push a constant onto the stack. */
#define PUSHC(OPERAND_TYPE, OPERAND_VALUE) \
  { int saw_index = 0;  int ival = (OPERAND_VALUE); \
    if (saw_index) java_push_constant_from_pool (current_jcf, ival); \
    else expand_java_pushc (ival, OPERAND_TYPE##_type_node); }

/* internal macro added for use by the WIDE case */
#define LOAD_INTERNAL(OPTYPE, OPVALUE) \
  push_value (find_local_variable (OPVALUE, type_map[OPVALUE], oldpc));

/* Push local variable onto the opcode stack. */
#define LOAD(OPERAND_TYPE, OPERAND_VALUE) \
  { \
    /* have to do this since OPERAND_VALUE may have side-effects */ \
    int opvalue = OPERAND_VALUE; \
    LOAD_INTERNAL(OPERAND_TYPE##_type_node, opvalue); \
  }

#define RETURN(OPERAND_TYPE, OPERAND_VALUE) \
  expand_java_return (OPERAND_TYPE##_type_node)

#define REM_EXPR TRUNC_MOD_EXPR
#define BINOP(OPERAND_TYPE, OPERAND_VALUE) \
  expand_java_binop (OPERAND_TYPE##_type_node, OPERAND_VALUE##_EXPR)

#define FIELD(IS_STATIC, IS_PUT) \
  expand_java_field_op (IS_STATIC, IS_PUT, IMMEDIATE_u2)

#define TEST(OPERAND_TYPE, CONDITION) \
  expand_test (CONDITION##_EXPR, OPERAND_TYPE##_type_node, oldpc+IMMEDIATE_s2)

#define COND(OPERAND_TYPE, CONDITION) \
  expand_cond (CONDITION##_EXPR, OPERAND_TYPE##_type_node, oldpc+IMMEDIATE_s2)

#define BRANCH(OPERAND_TYPE, OPERAND_VALUE) \
  BRANCH_##OPERAND_TYPE (OPERAND_VALUE)

#define BRANCH_GOTO(OPERAND_VALUE) \
  expand_java_goto (oldpc + OPERAND_VALUE)

#define BRANCH_CALL(OPERAND_VALUE) \
  expand_java_call (oldpc + OPERAND_VALUE, oldpc)

#if 0
#define BRANCH_RETURN(OPERAND_VALUE) \
  { \
    tree type = OPERAND_TYPE##_type_node; \
    tree value = find_local_variable (OPERAND_VALUE, type, oldpc); \
    expand_java_ret (value); \
  }
#endif

#define NOT_IMPL(OPERAND_TYPE, OPERAND_VALUE) \
	  fprintf (stderr, "%3d: %s ", oldpc, opname); \
	  fprintf (stderr, "(not implemented)\n")
#define NOT_IMPL1(OPERAND_VALUE) \
	  fprintf (stderr, "%3d: %s ", oldpc, opname); \
	  fprintf (stderr, "(not implemented)\n")

#define BRANCH_RETURN(OPERAND_VALUE) NOT_IMPL1(OPERAND_VALUE)

#define STACK(SUBOP, COUNT) STACK_##SUBOP (COUNT)

#define STACK_POP(COUNT) java_stack_pop (COUNT)

#define STACK_SWAP(COUNT) java_stack_swap()

#define STACK_DUP(COUNT) java_stack_dup (COUNT, 0)
#define STACK_DUPx1(COUNT) java_stack_dup (COUNT, 1)
#define STACK_DUPx2(COUNT) java_stack_dup (COUNT, 2)

#define SWITCH(OPERAND_TYPE, TABLE_OR_LOOKUP) \
  PC = (PC + 3) / 4 * 4; TABLE_OR_LOOKUP##_SWITCH

#define LOOKUP_SWITCH \
  { jint default_offset = IMMEDIATE_s4;  jint npairs = IMMEDIATE_s4; \
    tree selector = pop_value (INT_type_node); \
    tree duplicate, label; \
    tree type = TREE_TYPE (selector); \
    flush_quick_stack (); \
    expand_start_case (0, selector, type, "switch statement");\
    push_momentary (); \
    while (--npairs >= 0) \
      { \
	jint match = IMMEDIATE_s4; jint offset = IMMEDIATE_s4; \
	tree value = build_int_2 (match, match < 0 ? -1 : 0); \
	TREE_TYPE (value) = type; \
	label =  build_decl (LABEL_DECL, NULL_TREE, NULL_TREE); \
	pushcase (value, convert, label, &duplicate); \
	expand_java_goto (oldpc + offset); \
      } \
    label =  build_decl (LABEL_DECL, NULL_TREE, NULL_TREE); \
    pushcase (NULL_TREE, 0, label, &duplicate); \
    expand_java_goto (oldpc + default_offset); \
    pop_momentary (); \
    expand_end_case (selector); \
  }

#define TABLE_SWITCH \
  { jint default_offset = IMMEDIATE_s4; \
    jint low = IMMEDIATE_s4; jint high = IMMEDIATE_s4; \
    tree selector = pop_value (INT_type_node); \
    tree duplicate, label; \
    tree type = TREE_TYPE (selector); \
    flush_quick_stack (); \
    expand_start_case (0, selector, type, "switch statement");\
    push_momentary (); \
    for (; low <= high; low++) \
      { \
        jint offset = IMMEDIATE_s4; \
        tree value = build_int_2 (low, low < 0 ? -1 : 0); \
        TREE_TYPE (value) = type; \
        label =  build_decl (LABEL_DECL, NULL_TREE, NULL_TREE); \
        pushcase (value, convert, label, &duplicate); \
        expand_java_goto (oldpc + offset); \
      } \
    label =  build_decl (LABEL_DECL, NULL_TREE, NULL_TREE); \
    pushcase (NULL_TREE, 0, label, &duplicate); \
    expand_java_goto (oldpc + default_offset); \
    pop_momentary (); \
    expand_end_case (selector); \
  }

#define INVOKE(MAYBE_STATIC, IS_INTERFACE) \
  { int opcode = byte_ops[PC-1]; \
    int method_ref_index = IMMEDIATE_u2; \
    int nargs; \
    if (IS_INTERFACE) { nargs = IMMEDIATE_u1;  (void) IMMEDIATE_u1; } \
    else nargs = -1; \
    expand_invoke (opcode, method_ref_index, nargs); \
  }

/* Handle new, checkcast, instanceof */
#define OBJECT(TYPE, OP) \
  expand_java_##OP (get_class_constant (current_jcf, IMMEDIATE_u2))

#define ARRAY(OPERAND_TYPE, SUBOP) ARRAY_##SUBOP(OPERAND_TYPE)

#define ARRAY_LOAD(OPERAND_TYPE) 			\
  {							\
    expand_java_arrayload( OPERAND_TYPE##_type_node );	\
  }

#define ARRAY_STORE(OPERAND_TYPE)			\
  {							\
    expand_java_arraystore( OPERAND_TYPE##_type_node );	\
  }

#define ARRAY_LENGTH(OPERAND_TYPE) expand_java_array_length();
#define ARRAY_NEW(OPERAND_TYPE) ARRAY_NEW_##OPERAND_TYPE()
#define ARRAY_NEW_PTR()							\
    push_value (build_anewarray (get_class_constant (current_jcf,	\
						     IMMEDIATE_u2),	\
				 pop_value (int_type_node)));
#define ARRAY_NEW_NUM()				\
  {						\
    int atype = IMMEDIATE_u1;			\
    push_value (build_newarray (atype, pop_value (int_type_node)));\
  }
#define ARRAY_NEW_MULTI()					\
  {								\
    tree class = get_class_constant (current_jcf, IMMEDIATE_u2 );	\
    int  ndims = IMMEDIATE_u1;					\
    expand_java_multianewarray( class, ndims );			\
  }

#define UNOP(OPERAND_TYPE, OPERAND_VALUE) \
  push_value (fold (build1 (NEGATE_EXPR, OPERAND_TYPE##_type_node, \
			    pop_value (OPERAND_TYPE##_type_node))));

#define CONVERT2(FROM_TYPE, TO_TYPE)					 \
  {									 \
    push_value (build1 (NOP_EXPR, int_type_node,			 \
			(convert (TO_TYPE##_type_node,			 \
				  pop_value (FROM_TYPE##_type_node))))); \
  }

#define CONVERT(FROM_TYPE, TO_TYPE)				\
  {								\
    push_value (convert (TO_TYPE##_type_node,	                \
			 pop_value (FROM_TYPE##_type_node)));	\
  }

/* internal macro added for use by the WIDE case 
   Added TREE_TYPE (decl) assignment, apbianco  */
#define STORE_INTERNAL(OPTYPE, OPVALUE)			\
  {							\
    tree decl, value;					\
    int var = OPVALUE;					\
    tree type = OPTYPE;					\
    value = pop_value (type);				\
    type = TREE_TYPE (value);				\
    decl = find_local_variable (var, type, oldpc);	\
    set_local_type (var, type );			\
    expand_assignment (decl, value, 0, 0);		\
  }

#define STORE(OPERAND_TYPE, OPERAND_VALUE) \
  { \
    /* have to do this since OPERAND_VALUE may have side-effects */ \
    int opvalue = OPERAND_VALUE; \
    STORE_INTERNAL(OPERAND_TYPE##_type_node, opvalue); \
  }

#define SPECIAL(OPERAND_TYPE, INSTRUCTION) \
  SPECIAL_##INSTRUCTION(OPERAND_TYPE)

#define SPECIAL_ENTER(IGNORED) MONITOR_OPERATION (soft_monitorenter_node)
#define SPECIAL_EXIT(IGNORED)  MONITOR_OPERATION (soft_monitorexit_node)

#define MONITOR_OPERATION(call)			\
  {						\
    tree o = pop_value (ptr_type_node);		\
    tree c;					\
    flush_quick_stack ();			\
    c = build_java_monitor (call, o);		\
    TREE_SIDE_EFFECTS (c) = 1;			\
    expand_expr_stmt (c);			\
  }

#define SPECIAL_IINC(IGNORED) \
  { \
    unsigned int local_var_index = IMMEDIATE_u1; \
    int ival = IMMEDIATE_s1; \
    expand_iinc(local_var_index, ival, oldpc); \
  }

#define SPECIAL_WIDE(IGNORED) \
  { \
    int modified_opcode = IMMEDIATE_u1; \
    unsigned int local_var_index = IMMEDIATE_u2; \
    switch (modified_opcode) \
      { \
      case OPCODE_iinc: \
	{ \
	  int ival = IMMEDIATE_s2; \
	  expand_iinc (local_var_index, ival, oldpc); \
	  break; \
	} \
      case OPCODE_iload: \
      case OPCODE_lload: \
      case OPCODE_fload: \
      case OPCODE_dload: \
      case OPCODE_aload: \
	{ \
	  /* duplicate code from LOAD macro */ \
	  LOAD_INTERNAL(operand_type[modified_opcode], local_var_index); \
	  break; \
	} \
      case OPCODE_istore: \
      case OPCODE_lstore: \
      case OPCODE_fstore: \
      case OPCODE_dstore: \
      case OPCODE_astore: \
	{ \
	  STORE_INTERNAL(operand_type[modified_opcode], local_var_index); \
	  break; \
	} \
      default: \
        error ("unrecogized wide sub-instruction"); \
      } \
  }

#define SPECIAL_THROW(IGNORED) \
  build_java_athrow (pop_value (throwable_type_node))

#define SPECIAL_BREAK NOT_IMPL1
#define IMPL          NOT_IMPL

#include "javaop.def"
#undef JAVAOP
   default:
    fprintf (stderr, "%3d: unknown(%3d)\n", oldpc, byte_ops[PC]);
  }
  return PC;
}

/* Force the (direct) sub-operands of NODE to be evaluated in left-to-right
   order, as specified by Java Language Specification.

   The problem is that while expand_expr will evaluate its sub-operands in
   left-to-right order, for variables it will just return an rtx (i.e.
   an lvalue) for the variable (rather than an rvalue).  So it is possible
   that a later sub-operand will change the register, and when the
   actual operation is done, it will use the new value, when it should
   have used the original value.

   We fix this by using save_expr.  This forces the sub-operand to be
   copied into a fresh virtual register,
*/

tree
force_evaluation_order (node)
     tree  node;
{
  if (flag_syntax_only)
    return node;
  if (TREE_CODE_CLASS (TREE_CODE (node)) == '2')
    {
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (node, 1)))
	TREE_OPERAND (node, 0) = save_expr (TREE_OPERAND (node, 0));
    }
  else if (TREE_CODE (node) == CALL_EXPR || TREE_CODE (node) == NEW_CLASS_EXPR)
    {
      tree last_side_effecting_arg = NULL_TREE;
      tree arg = TREE_OPERAND (node, 1);
      for (; arg != NULL_TREE; arg = TREE_CHAIN (arg))
	{
	  if (TREE_SIDE_EFFECTS (TREE_VALUE (arg)))
	    last_side_effecting_arg = arg;
	}
      arg = TREE_OPERAND (node, 1);
      for (; arg != NULL_TREE;  arg = TREE_CHAIN (arg))
	{
	  if (arg == last_side_effecting_arg)
	    break;
	  TREE_VALUE (arg) = save_expr (TREE_VALUE (arg)); 
	}
    }
  return node;
}
