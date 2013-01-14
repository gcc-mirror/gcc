/* Process expressions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"			/* For INT_TYPE_SIZE,
				   TARGET_VTABLE_USES_DESCRIPTORS,
				   BITS_PER_UNIT,
				   MODIFY_JNI_METHOD_CALL and
				   PARM_BOUNDARY.  */
				   
#include "tree.h"
#include "flags.h"
#include "java-tree.h"
#include "javaop.h"
#include "java-opcodes.h"
#include "jcf.h"
#include "java-except.h"
#include "parse.h"
#include "diagnostic-core.h"
#include "ggc.h"
#include "tree-iterator.h"
#include "target.h"

static void flush_quick_stack (void);
static void push_value (tree);
static tree pop_value (tree);
static void java_stack_swap (void);
static void java_stack_dup (int, int);
static void build_java_athrow (tree);
static void build_java_jsr (int, int);
static void build_java_ret (tree);
static void expand_java_multianewarray (tree, int);
static void expand_java_arraystore (tree);
static void expand_java_arrayload (tree);
static void expand_java_array_length (void);
static tree build_java_monitor (tree, tree);
static void expand_java_pushc (int, tree);
static void expand_java_return (tree);
static void expand_load_internal (int, tree, int);
static void expand_java_NEW (tree);
static void expand_java_INSTANCEOF (tree);
static void expand_java_CHECKCAST (tree);
static void expand_iinc (unsigned int, int, int);
static void expand_java_binop (tree, enum tree_code);
static void note_label (int, int);
static void expand_compare (enum tree_code, tree, tree, int);
static void expand_test (enum tree_code, tree, int);
static void expand_cond (enum tree_code, tree, int);
static void expand_java_goto (int);
static tree expand_java_switch (tree, int);
static void expand_java_add_case (tree, int, int);
static vec<tree, va_gc> *pop_arguments (tree); 
static void expand_invoke (int, int, int); 
static void expand_java_field_op (int, int, int); 
static void java_push_constant_from_pool (struct JCF *, int); 
static void java_stack_pop (int); 
static tree build_java_throw_out_of_bounds_exception (tree); 
static tree build_java_check_indexed_type (tree, tree); 
static unsigned char peek_opcode_at_pc (struct JCF *, int, int);
static void promote_arguments (void);
static void cache_cpool_data_ref (void);

static GTY(()) tree operand_type[59];

static GTY(()) tree methods_ident;
static GTY(()) tree ncode_ident;
tree dtable_ident = NULL_TREE;

/* Set to nonzero value in order to emit class initialization code
   before static field references.  */
int always_initialize_class_p = 0;

/* We store the stack state in two places:
   Within a basic block, we use the quick_stack, which is a vec of expression
   nodes.
   This is the top part of the stack;  below that we use find_stack_slot.
   At the end of a basic block, the quick_stack must be flushed
   to the stack slot array (as handled by find_stack_slot).
   Using quick_stack generates better code (especially when
   compiled without optimization), because we do not have to
   explicitly store and load trees to temporary variables.

   If a variable is on the quick stack, it means the value of variable
   when the quick stack was last flushed.  Conceptually, flush_quick_stack
   saves all the quick_stack elements in parallel.  However, that is
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

static GTY(()) vec<tree, va_gc> *quick_stack;

/* The physical memory page size used in this computer.  See
   build_field_ref().  */
static GTY(()) tree page_size;

/* The stack pointer of the Java virtual machine.
   This does include the size of the quick_stack. */

int stack_pointer;

const unsigned char *linenumber_table;
int linenumber_count;

/* Largest pc so far in this method that has been passed to lookup_label. */
int highest_label_pc_this_method = -1;

/* Base value for this method to add to pc to get generated label. */
int start_label_pc_this_method = 0;

void
init_expr_processing (void)
{
  operand_type[21] = operand_type[54] = int_type_node;
  operand_type[22] = operand_type[55] = long_type_node;
  operand_type[23] = operand_type[56] = float_type_node;
  operand_type[24] = operand_type[57] = double_type_node;
  operand_type[25] = operand_type[58] = ptr_type_node;
}

tree
java_truthvalue_conversion (tree expr)
{
  /* It is simpler and generates better code to have only TRUTH_*_EXPR
     or comparison expressions as truth values at this level.

     This function should normally be identity for Java.  */

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:   case NE_EXPR:   case UNEQ_EXPR: case LTGT_EXPR:
    case LE_EXPR:   case GE_EXPR:   case LT_EXPR:   case GT_EXPR:
    case UNLE_EXPR: case UNGE_EXPR: case UNLT_EXPR: case UNGT_EXPR:
    case ORDERED_EXPR: case UNORDERED_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
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
      /* These don't change whether an object is nonzero or zero.  */
      return java_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold_build3 (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
			  java_truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  java_truthvalue_conversion (TREE_OPERAND (expr, 2)));

    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
          >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
        return java_truthvalue_conversion (TREE_OPERAND (expr, 0));
      /* fall through to default */

    default:
      return fold_build2 (NE_EXPR, boolean_type_node,
			  expr, boolean_false_node);
    }
}

/* Save any stack slots that happen to be in the quick_stack into their
   home virtual register slots.

   The copy order is from low stack index to high, to support the invariant
   that the expression for a slot may contain decls for stack slots with
   higher (or the same) index, but not lower. */

static void
flush_quick_stack (void)
{
  int stack_index = stack_pointer;
  unsigned ix;
  tree t;

  /* Count the number of slots the quick stack is holding.  */
  for (ix = 0; vec_safe_iterate (quick_stack, ix, &t); ix++)
    stack_index -= 1 + TYPE_IS_WIDE (TREE_TYPE (t));

  for (ix = 0; vec_safe_iterate (quick_stack, ix, &t); ix++)
    {
      tree decl, type = TREE_TYPE (t);

      decl = find_stack_slot (stack_index, type);
      if (decl != t)
	java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (t), decl, t));
      stack_index += 1 + TYPE_IS_WIDE (type);
    }

  vec_safe_truncate (quick_stack, 0);
}

/* Push TYPE on the type stack.
   Return true on success, 0 on overflow. */

int
push_type_0 (tree type)
{
  int n_words;
  type = promote_type (type);
  n_words = 1 + TYPE_IS_WIDE (type);
  if (stack_pointer + n_words > DECL_MAX_STACK (current_function_decl))
    return 0;
  /* Allocate decl for this variable now, so we get a temporary that
     survives the whole method. */
  find_stack_slot (stack_pointer, type);
  stack_type_map[stack_pointer++] = type;
  n_words--;
  while (--n_words >= 0)
    stack_type_map[stack_pointer++] = TYPE_SECOND;
  return 1;
}

void
push_type (tree type)
{
  int r = push_type_0 (type);
  gcc_assert (r);
}

static void
push_value (tree value)
{
  tree type = TREE_TYPE (value);
  if (TYPE_PRECISION (type) < 32 && INTEGRAL_TYPE_P (type))
    {
      type = promote_type (type);
      value = convert (type, value);
    }
  push_type (type);
  vec_safe_push (quick_stack, value);

  /* If the value has a side effect, then we need to evaluate it
     whether or not the result is used.  If the value ends up on the
     quick stack and is then popped, this won't happen -- so we flush
     the quick stack.  It is safest to simply always flush, though,
     since TREE_SIDE_EFFECTS doesn't capture COMPONENT_REF, and for
     the latter we may need to strip conversions.  */
  flush_quick_stack ();
}

/* Pop a type from the type stack.
   TYPE is the expected type.   Return the actual type, which must be
   convertible to TYPE.
   On an error, *MESSAGEP is set to a freshly malloc'd error message. */

tree
pop_type_0 (tree type, char **messagep)
{
  int n_words;
  tree t;
  *messagep = NULL;
  if (TREE_CODE (type) == RECORD_TYPE)
    type = promote_type (type);
  n_words = 1 + TYPE_IS_WIDE (type);
  if (stack_pointer < n_words)
    {
      *messagep = xstrdup ("stack underflow");
      return type;
    }
  while (--n_words > 0)
    {
      if (stack_type_map[--stack_pointer] != void_type_node)
	{
	  *messagep = xstrdup ("Invalid multi-word value on type stack");
	  return type;
	}
    }
  t = stack_type_map[--stack_pointer];
  if (type == NULL_TREE || t == type)
    return t;
  if (TREE_CODE (t) == TREE_LIST)
    {      
      do
	{
	  tree tt = TREE_PURPOSE (t);
	  if (! can_widen_reference_to (tt, type))
	    {
	      t = tt;
	      goto fail;
	    }
	  t = TREE_CHAIN (t);
	}
      while (t);
      return t;
    }
  if (INTEGRAL_TYPE_P (type) && INTEGRAL_TYPE_P (t)
      && TYPE_PRECISION (type) <= 32 && TYPE_PRECISION (t) <= 32)
    return t;
  if (TREE_CODE (type) == POINTER_TYPE && TREE_CODE (t) == POINTER_TYPE)
    {
      /* If the expected type we've been passed is object or ptr
	 (i.e. void*), the caller needs to know the real type.  */
      if (type == ptr_type_node || type == object_ptr_type_node)
        return t;

      /* Since the verifier has already run, we know that any
	 types we see will be compatible.  In BC mode, this fact
	 may be checked at runtime, but if that is so then we can
	 assume its truth here as well.  So, we always succeed
	 here, with the expected type.  */
      return type;
    }

  if (! flag_verify_invocations && flag_indirect_dispatch
      && t == object_ptr_type_node)
    {
      if (type != ptr_type_node)
	warning (0, "need to insert runtime check for %s", 
		 xstrdup (lang_printable_name (type, 0)));
      return type;
    }

  /* lang_printable_name uses a static buffer, so we must save the result
     from calling it the first time.  */
 fail:
  {
    char *temp = xstrdup (lang_printable_name (type, 0));
    /* If the stack contains a multi-word type, keep popping the stack until 
       the real type is found.  */
    while (t == void_type_node)
      t = stack_type_map[--stack_pointer];
    *messagep = concat ("expected type '", temp,
			"' but stack contains '", lang_printable_name (t, 0),
			"'", NULL);
    free (temp);
  }
  return type;
}

/* Pop a type from the type stack.
   TYPE is the expected type.  Return the actual type, which must be
   convertible to TYPE, otherwise call error. */

tree
pop_type (tree type)
{
  char *message = NULL;
  type = pop_type_0 (type, &message);
  if (message != NULL)
    {
      error ("%s", message);
      free (message);
    }
  return type;
}


/* Return true if two type assertions are equal.  */

static int
type_assertion_eq (const void * k1_p, const void * k2_p)
{
  const type_assertion k1 = *(const type_assertion *)k1_p;
  const type_assertion k2 = *(const type_assertion *)k2_p;
  return (k1.assertion_code == k2.assertion_code
          && k1.op1 == k2.op1
	  && k1.op2 == k2.op2);
}

/* Hash a type assertion.  */

static hashval_t
type_assertion_hash (const void *p)
{
  const type_assertion *k_p = (const type_assertion *) p;
  hashval_t hash = iterative_hash (&k_p->assertion_code, sizeof
				   k_p->assertion_code, 0);

  switch (k_p->assertion_code)
    {
    case JV_ASSERT_TYPES_COMPATIBLE:
      hash = iterative_hash (&TYPE_UID (k_p->op2), sizeof TYPE_UID (k_p->op2),
			     hash);
      /* Fall through.  */

    case JV_ASSERT_IS_INSTANTIABLE:
      hash = iterative_hash (&TYPE_UID (k_p->op1), sizeof TYPE_UID (k_p->op1),
			     hash);
      /* Fall through.  */

    case JV_ASSERT_END_OF_TABLE:
      break;

    default:
      gcc_unreachable ();
    }

  return hash;
}

/* Add an entry to the type assertion table for the given class.  
   KLASS is the class for which this assertion will be evaluated by the 
   runtime during loading/initialization.
   ASSERTION_CODE is the 'opcode' or type of this assertion: see java-tree.h.
   OP1 and OP2 are the operands. The tree type of these arguments may be
   specific to each assertion_code. */

void
add_type_assertion (tree klass, int assertion_code, tree op1, tree op2)
{
  htab_t assertions_htab;
  type_assertion as;
  void **as_pp;

  assertions_htab = TYPE_ASSERTIONS (klass);
  if (assertions_htab == NULL)
    {
      assertions_htab = htab_create_ggc (7, type_assertion_hash, 
					 type_assertion_eq, NULL);
      TYPE_ASSERTIONS (current_class) = assertions_htab;
    }

  as.assertion_code = assertion_code;
  as.op1 = op1;
  as.op2 = op2;

  as_pp = htab_find_slot (assertions_htab, &as, INSERT);

  /* Don't add the same assertion twice.  */
  if (*as_pp)
    return;

  *as_pp = ggc_alloc_type_assertion ();
  **(type_assertion **)as_pp = as;
}


/* Return 1 if SOURCE_TYPE can be safely widened to TARGET_TYPE.
   Handles array types and interfaces.  */

int
can_widen_reference_to (tree source_type, tree target_type)
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

  /* FIXME: This is very pessimistic, in that it checks everything,
     even if we already know that the types are compatible.  If we're
     to support full Java class loader semantics, we need this.
     However, we could do something more optimal.  */
  if (! flag_verify_invocations)
    {
      add_type_assertion (current_class, JV_ASSERT_TYPES_COMPATIBLE, 
			  source_type, target_type);

      if (!quiet_flag)
       warning (0, "assert: %s is assign compatible with %s", 
		xstrdup (lang_printable_name (target_type, 0)),
		xstrdup (lang_printable_name (source_type, 0)));
      /* Punt everything to runtime.  */
      return 1;
    }

  if (TYPE_DUMMY (source_type) || TYPE_DUMMY (target_type))
    {
      return 1;
    }
  else
    {
      if (TYPE_ARRAY_P (source_type) || TYPE_ARRAY_P (target_type))
	{
	  HOST_WIDE_INT source_length, target_length;
	  if (TYPE_ARRAY_P (source_type) != TYPE_ARRAY_P (target_type))
	    {
	      /* An array implements Cloneable and Serializable.  */
	      tree name = DECL_NAME (TYPE_NAME (target_type));
	      return (name == java_lang_cloneable_identifier_node
		      || name == java_io_serializable_identifier_node);
	    }
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

	  if (TYPE_DUMMY (source_type) || TYPE_DUMMY (target_type))
	    {
	      if (! quiet_flag)
		warning (0, "assert: %s is assign compatible with %s", 
			 xstrdup (lang_printable_name (target_type, 0)),
			 xstrdup (lang_printable_name (source_type, 0)));
	      return 1;
	    }

 	  /* class_depth can return a negative depth if an error occurred */
	  if (source_depth < 0 || target_depth < 0)
	    return 0;

	  if (CLASS_INTERFACE (TYPE_NAME (target_type)))
	    {
	      /* target_type is OK if source_type or source_type ancestors
		 implement target_type. We handle multiple sub-interfaces  */
	      tree binfo, base_binfo;
	      int i;

	      for (binfo = TYPE_BINFO (source_type), i = 0;
		   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	        if (can_widen_reference_to
		    (BINFO_TYPE (base_binfo), target_type))
		  return 1;
	      
	      if (!i)
		return 0;
	    }

	  for ( ; source_depth > target_depth;  source_depth--) 
	    {
	      source_type
		= BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (source_type), 0));
	    }
	  return source_type == target_type;
	}
    }
}

static tree
pop_value (tree type)
{
  type = pop_type (type);
  if (vec_safe_length (quick_stack) != 0)
    return quick_stack->pop ();
  else
    return find_stack_slot (stack_pointer, promote_type (type));
}


/* Pop and discard the top COUNT stack slots. */

static void
java_stack_pop (int count)
{
  while (count > 0)
    {
      tree type;

      gcc_assert (stack_pointer != 0);

      type = stack_type_map[stack_pointer - 1];
      if (type == TYPE_SECOND)
	{
	  count--;
	  gcc_assert (stack_pointer != 1 && count > 0);

	  type = stack_type_map[stack_pointer - 2];
	}
      pop_value (type);
      count--;
    }
}

/* Implement the 'swap' operator (to swap two top stack slots). */

static void
java_stack_swap (void)
{
  tree type1, type2;
  tree temp;
  tree decl1, decl2;

  if (stack_pointer < 2
      || (type1 = stack_type_map[stack_pointer - 1]) == TYPE_SECOND
      || (type2 = stack_type_map[stack_pointer - 2]) == TYPE_SECOND
      || TYPE_IS_WIDE (type1) || TYPE_IS_WIDE (type2))
    /* Bad stack swap.  */
    abort ();
  /* Bad stack swap.  */

  flush_quick_stack ();
  decl1 = find_stack_slot (stack_pointer - 1, type1);
  decl2 = find_stack_slot (stack_pointer - 2, type2);
  temp = build_decl (input_location, VAR_DECL, NULL_TREE, type1);
  java_add_local_var (temp);
  java_add_stmt (build2 (MODIFY_EXPR, type1, temp, decl1));
  java_add_stmt (build2 (MODIFY_EXPR, type2, 
			 find_stack_slot (stack_pointer - 1, type2),
			 decl2));
  java_add_stmt (build2 (MODIFY_EXPR, type1, 
			 find_stack_slot (stack_pointer - 2, type1),
			 temp));
  stack_type_map[stack_pointer - 1] = type2;
  stack_type_map[stack_pointer - 2] = type1;
}

static void
java_stack_dup (int size, int offset)
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
	  /* Dup operation splits 64-bit number.  */
	  gcc_assert (src_index > low_index);

	  stack_type_map[dst_index] = type;
	  src_index--;  dst_index--;
	  type = stack_type_map[src_index];
	  gcc_assert (TYPE_IS_WIDE (type));
	}
      else
	gcc_assert (! TYPE_IS_WIDE (type));

      if (src_index != dst_index)
	{
	  tree src_decl = find_stack_slot (src_index, type);
	  tree dst_decl = find_stack_slot (dst_index, type);

	  java_add_stmt 
	    (build2 (MODIFY_EXPR, TREE_TYPE (dst_decl), dst_decl, src_decl));
	  stack_type_map[dst_index] = type;
	}
    }
}

/* Calls _Jv_Throw or _Jv_Sjlj_Throw.  Discard the contents of the
   value stack. */

static void
build_java_athrow (tree node)
{
  tree call;

  call = build_call_nary (void_type_node,
			  build_address_of (throw_node),
			  1, node);
  TREE_SIDE_EFFECTS (call) = 1;
  java_add_stmt (call);
  java_stack_pop (stack_pointer);
}

/* Implementation for jsr/ret */

static void
build_java_jsr (int target_pc, int return_pc)
{
  tree where =  lookup_label (target_pc);
  tree ret = lookup_label (return_pc);
  tree ret_label = fold_build1 (ADDR_EXPR, return_address_type_node, ret);
  push_value (ret_label);
  flush_quick_stack ();
  java_add_stmt (build1 (GOTO_EXPR, void_type_node, where));

  /* Do not need to emit the label here.  We noted the existence of the
     label as a jump target in note_instructions; we'll emit the label
     for real at the beginning of the expand_byte_code loop.  */
}

static void
build_java_ret (tree location)
{
  java_add_stmt (build1 (GOTO_EXPR, void_type_node, location));
}
 
/* Implementation of operations on array: new, load, store, length */

tree
decode_newarray_type (int atype)
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
encode_newarray_type (tree type)
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
    gcc_unreachable ();
}

/* Build a call to _Jv_ThrowBadArrayIndex(), the
   ArrayIndexOfBoundsException exception handler.  */

static tree
build_java_throw_out_of_bounds_exception (tree index)
{
  tree node;

  /* We need to build a COMPOUND_EXPR because _Jv_ThrowBadArrayIndex()
     has void return type.  We cannot just set the type of the CALL_EXPR below
     to int_type_node because we would lose it during gimplification.  */
  gcc_assert (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (soft_badarrayindex_node))));
  node = build_call_nary (void_type_node,
			       build_address_of (soft_badarrayindex_node),
			       1, index);
  TREE_SIDE_EFFECTS (node) = 1;

  node = build2 (COMPOUND_EXPR, int_type_node, node, integer_zero_node);
  TREE_SIDE_EFFECTS (node) = 1;	/* Allows expansion within ANDIF */

  return (node);
}

/* Return the length of an array. Doesn't perform any checking on the nature
   or value of the array NODE. May be used to implement some bytecodes.  */

tree
build_java_array_length_access (tree node)
{
  tree type = TREE_TYPE (node);
  tree array_type = TREE_TYPE (type);
  HOST_WIDE_INT length;

  if (!is_array_type_p (type))
    {
      /* With the new verifier, we will see an ordinary pointer type
	 here.  In this case, we just use an arbitrary array type.  */
      array_type = build_java_array_type (object_ptr_type_node, -1);
      type = promote_type (array_type);
    }

  length = java_array_type_length (type);
  if (length >= 0)
    return build_int_cst (NULL_TREE, length);

  node = build3 (COMPONENT_REF, int_type_node,
		 build_java_indirect_ref (array_type, node,
					  flag_check_references),
		 lookup_field (&array_type, get_identifier ("length")),
		 NULL_TREE);
  IS_ARRAY_LENGTH_ACCESS (node) = 1;
  return node;
}

/* Optionally checks a reference against the NULL pointer.  ARG1: the
   expr, ARG2: we should check the reference.  Don't generate extra
   checks if we're not generating code.  */

tree 
java_check_reference (tree expr, int check)
{
  if (!flag_syntax_only && check)
    {
      expr = save_expr (expr);
      expr = build3 (COND_EXPR, TREE_TYPE (expr),
		     build2 (EQ_EXPR, boolean_type_node,
			     expr, null_pointer_node),
		     build_call_nary (void_type_node, 
				      build_address_of (soft_nullpointer_node),
				      0),
		     expr);
    }

  return expr;
}

/* Reference an object: just like an INDIRECT_REF, but with checking.  */

tree
build_java_indirect_ref (tree type, tree expr, int check)
{
  tree t;
  t = java_check_reference (expr, check);
  t = convert (build_pointer_type (type), t);
  return build1 (INDIRECT_REF, type, t);
}

/* Implement array indexing (either as l-value or r-value).
   Returns a tree for ARRAY[INDEX], assume TYPE is the element type.
   Optionally performs bounds checking and/or test to NULL.
   At this point, ARRAY should have been verified as an array.  */

tree
build_java_arrayaccess (tree array, tree type, tree index)
{
  tree node, throw_expr = NULL_TREE;
  tree data_field;
  tree ref;
  tree array_type = TREE_TYPE (TREE_TYPE (array));
  tree size_exp = fold_convert (sizetype, size_in_bytes (type));

  if (!is_array_type_p (TREE_TYPE (array)))
    {
      /* With the new verifier, we will see an ordinary pointer type
	 here.  In this case, we just use the correct array type.  */
      array_type = build_java_array_type (type, -1);
    }

  if (flag_bounds_check)
    {
      /* Generate:
       * (unsigned jint) INDEX >= (unsigned jint) LEN
       *    && throw ArrayIndexOutOfBoundsException.
       * Note this is equivalent to and more efficient than:
       * INDEX < 0 || INDEX >= LEN && throw ... */
      tree test;
      tree len = convert (unsigned_int_type_node,
			  build_java_array_length_access (array));
      test = fold_build2 (GE_EXPR, boolean_type_node, 
			  convert (unsigned_int_type_node, index),
			  len);
      if (! integer_zerop (test))
	{
	  throw_expr
	    = build2 (TRUTH_ANDIF_EXPR, int_type_node, test,
		      build_java_throw_out_of_bounds_exception (index));
	  /* allows expansion within COMPOUND */
	  TREE_SIDE_EFFECTS( throw_expr ) = 1;
	}
    }

  /* If checking bounds, wrap the index expr with a COMPOUND_EXPR in order
     to have the bounds check evaluated first. */
  if (throw_expr != NULL_TREE)
    index = build2 (COMPOUND_EXPR, int_type_node, throw_expr, index);

  data_field = lookup_field (&array_type, get_identifier ("data"));

  ref = build3 (COMPONENT_REF, TREE_TYPE (data_field),    
		build_java_indirect_ref (array_type, array, 
					 flag_check_references),
		data_field, NULL_TREE);

  /* Take the address of the data field and convert it to a pointer to
     the element type.  */
  node = build1 (NOP_EXPR, build_pointer_type (type), build_address_of (ref));

  /* Multiply the index by the size of an element to obtain a byte
     offset.  Convert the result to a pointer to the element type.  */
  index = build2 (MULT_EXPR, sizetype, 
		  fold_convert (sizetype, index), 
		  size_exp);

  /* Sum the byte offset and the address of the data field.  */
  node = fold_build_pointer_plus (node, index);

  /* Finally, return

    *((&array->data) + index*size_exp)

  */
  return build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (node)), node);
}

/* Generate code to throw an ArrayStoreException if OBJECT is not assignable
   (at runtime) to an element of ARRAY.  A NOP_EXPR is returned if it can
   determine that no check is required. */

tree
build_java_arraystore_check (tree array, tree object)
{
  tree check, element_type, source;
  tree array_type_p = TREE_TYPE (array);
  tree object_type = TYPE_NAME (TREE_TYPE (TREE_TYPE (object)));

  if (! flag_verify_invocations)
    {
      /* With the new verifier, we don't track precise types.  FIXME:
	 performance regression here.  */
      element_type = TYPE_NAME (object_type_node);
    }
  else
    {
      gcc_assert (is_array_type_p (array_type_p));

      /* Get the TYPE_DECL for ARRAY's element type. */
      element_type
	= TYPE_NAME (TREE_TYPE (TREE_TYPE (TREE_TYPE (array_type_p))));
    }

  gcc_assert (TREE_CODE (element_type) == TYPE_DECL
	      && TREE_CODE (object_type) == TYPE_DECL);

  if (!flag_store_check)
    return build1 (NOP_EXPR, array_type_p, array);

  /* No check is needed if the element type is final.  Also check that
     element_type matches object_type, since in the bytecode
     compilation case element_type may be the actual element type of
     the array rather than its declared type.  However, if we're doing
     indirect dispatch, we can't do the `final' optimization.  */
  if (element_type == object_type
      && ! flag_indirect_dispatch
      && CLASS_FINAL (element_type))
    return build1 (NOP_EXPR, array_type_p, array);
  
  /* OBJECT might be wrapped by a SAVE_EXPR. */
  if (TREE_CODE (object) == SAVE_EXPR)
    source = TREE_OPERAND (object, 0);
  else
    source = object;
  
  /* Avoid the check if OBJECT was just loaded from the same array. */
  if (TREE_CODE (source) == ARRAY_REF)
    {
      tree target;
      source = TREE_OPERAND (source, 0); /* COMPONENT_REF. */
      source = TREE_OPERAND (source, 0); /* INDIRECT_REF. */
      source = TREE_OPERAND (source, 0); /* Source array's DECL or SAVE_EXPR. */
      if (TREE_CODE (source) == SAVE_EXPR)
	source = TREE_OPERAND (source, 0);
      
      target = array;
      if (TREE_CODE (target) == SAVE_EXPR)
	target = TREE_OPERAND (target, 0);
      
      if (source == target)
        return build1 (NOP_EXPR, array_type_p, array);
    }

  /* Build an invocation of _Jv_CheckArrayStore */
  check = build_call_nary (void_type_node,
			   build_address_of (soft_checkarraystore_node),
			   2, array, object);
  TREE_SIDE_EFFECTS (check) = 1;

  return check;
}

/* Makes sure that INDEXED_TYPE is appropriate. If not, make it from
   ARRAY_NODE. This function is used to retrieve something less vague than
   a pointer type when indexing the first dimension of something like [[<t>.
   May return a corrected type, if necessary, otherwise INDEXED_TYPE is
   return unchanged.  */

static tree
build_java_check_indexed_type (tree array_node ATTRIBUTE_UNUSED,
			       tree indexed_type)
{
  /* We used to check to see if ARRAY_NODE really had array type.
     However, with the new verifier, this is not necessary, as we know
     that the object will be an array of the appropriate type.  */

  return indexed_type;
}

/* newarray triggers a call to _Jv_NewPrimArray. This function should be 
   called with an integer code (the type of array to create), and the length
   of the array to create.  */

tree
build_newarray (int atype_value, tree length)
{
  tree type_arg;

  tree prim_type = decode_newarray_type (atype_value);
  tree type
    = build_java_array_type (prim_type,
			     host_integerp (length, 0) == INTEGER_CST
			     ? tree_low_cst (length, 0) : -1);

  /* Pass a reference to the primitive type class and save the runtime
     some work.  */
  type_arg = build_class_ref (prim_type);

  return build_call_nary (promote_type (type),
			  build_address_of (soft_newarray_node),
			  2, type_arg, length);
}

/* Generates anewarray from a given CLASS_TYPE. Gets from the stack the size
   of the dimension. */

tree
build_anewarray (tree class_type, tree length)
{
  tree type
    = build_java_array_type (class_type,
			     host_integerp (length, 0)
			     ? tree_low_cst (length, 0) : -1);

  return build_call_nary (promote_type (type),
			  build_address_of (soft_anewarray_node),
			  3,
			  length,
			  build_class_ref (class_type),
			  null_pointer_node);
}

/* Return a node the evaluates 'new TYPE[LENGTH]'. */

tree
build_new_array (tree type, tree length)
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
expand_java_multianewarray (tree class_type, int ndim)
{
  int i;
  vec<tree, va_gc> *args = NULL;

  vec_safe_grow (args, 3 + ndim);

  (*args)[0] = build_class_ref (class_type);
  (*args)[1] = build_int_cst (NULL_TREE, ndim);

  for(i = ndim - 1; i >= 0; i-- )
    (*args)[(unsigned)(2 + i)] = pop_value (int_type_node);

  (*args)[2 + ndim] = null_pointer_node;

  push_value (build_call_vec (promote_type (class_type),
                              build_address_of (soft_multianewarray_node),
                              args));
}

/*  ARRAY[INDEX] <- RHS. build_java_check_indexed_type makes sure that
    ARRAY is an array type. May expand some bound checking and NULL
    pointer checking. RHS_TYPE_NODE we are going to store. In the case
    of the CHAR/BYTE/BOOLEAN SHORT, the type popped of the stack is an
    INT. In those cases, we make the conversion.

    if ARRAy is a reference type, the assignment is checked at run-time
    to make sure that the RHS can be assigned to the array element
    type. It is not necessary to generate this code if ARRAY is final.  */

static void
expand_java_arraystore (tree rhs_type_node)
{
  tree rhs_node    = pop_value ((INTEGRAL_TYPE_P (rhs_type_node) 
				 && TYPE_PRECISION (rhs_type_node) <= 32) ? 
				 int_type_node : rhs_type_node);
  tree index = pop_value (int_type_node);
  tree array_type, array, temp, access;

  /* If we're processing an `aaload' we might as well just pick
     `Object'.  */
  if (TREE_CODE (rhs_type_node) == POINTER_TYPE)
    {
      array_type = build_java_array_type (object_ptr_type_node, -1);
      rhs_type_node = object_ptr_type_node;
    }
  else
    array_type = build_java_array_type (rhs_type_node, -1);

  array = pop_value (array_type);
  array = build1 (NOP_EXPR, promote_type (array_type), array);

  rhs_type_node    = build_java_check_indexed_type (array, rhs_type_node);

  flush_quick_stack ();

  index = save_expr (index);
  array = save_expr (array);

  /* We want to perform the bounds check (done by
     build_java_arrayaccess) before the type check (done by
     build_java_arraystore_check).  So, we call build_java_arrayaccess
     -- which returns an ARRAY_REF lvalue -- and we then generate code
     to stash the address of that lvalue in a temp.  Then we call
     build_java_arraystore_check, and finally we generate a
     MODIFY_EXPR to set the array element.  */

  access = build_java_arrayaccess (array, rhs_type_node, index);
  temp = build_decl (input_location, VAR_DECL, NULL_TREE, 
		     build_pointer_type (TREE_TYPE (access)));
  java_add_local_var (temp);
  java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (temp),
			 temp, 
			 build_fold_addr_expr (access)));

  if (TREE_CODE (rhs_type_node) == POINTER_TYPE)
    {
      tree check = build_java_arraystore_check (array, rhs_node);
      java_add_stmt (check);
    }
  
  java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (access), 
			 build1 (INDIRECT_REF, TREE_TYPE (access), temp),
			 rhs_node));  
}

/* Expand the evaluation of ARRAY[INDEX]. build_java_check_indexed_type makes 
   sure that LHS is an array type. May expand some bound checking and NULL
   pointer checking.  
   LHS_TYPE_NODE is the type of ARRAY[INDEX]. But in the case of CHAR/BYTE/
   BOOLEAN/SHORT, we push a promoted type back to the stack.
*/

static void
expand_java_arrayload (tree lhs_type_node)
{
  tree load_node;
  tree index_node = pop_value (int_type_node);
  tree array_type;
  tree array_node;

  /* If we're processing an `aaload' we might as well just pick
     `Object'.  */
  if (TREE_CODE (lhs_type_node) == POINTER_TYPE)
    {
      array_type = build_java_array_type (object_ptr_type_node, -1);
      lhs_type_node = object_ptr_type_node;
    }
  else
    array_type = build_java_array_type (lhs_type_node, -1);
  array_node = pop_value (array_type);
  array_node = build1 (NOP_EXPR, promote_type (array_type), array_node);

  index_node = save_expr (index_node);
  array_node = save_expr (array_node);

  lhs_type_node = build_java_check_indexed_type (array_node,
						 lhs_type_node);
  load_node = build_java_arrayaccess (array_node,
				      lhs_type_node,
				      index_node);
  if (INTEGRAL_TYPE_P (lhs_type_node) && TYPE_PRECISION (lhs_type_node) <= 32)
    load_node = fold_build1 (NOP_EXPR, int_type_node, load_node);
  push_value (load_node);
}

/* Expands .length. Makes sure that we deal with and array and may expand
   a NULL check on the array object.  */

static void
expand_java_array_length (void)
{
  tree array  = pop_value (ptr_type_node);
  tree length = build_java_array_length_access (array);

  push_value (length);
}

/* Emit code for the call to _Jv_Monitor{Enter,Exit}. CALL can be
   either soft_monitorenter_node or soft_monitorexit_node.  */

static tree
build_java_monitor (tree call, tree object)
{
  return build_call_nary (void_type_node,
			  build_address_of (call),
			  1, object);
}

/* Emit code for one of the PUSHC instructions. */

static void
expand_java_pushc (int ival, tree type)
{
  tree value;
  if (type == ptr_type_node && ival == 0)
    value = null_pointer_node;
  else if (type == int_type_node || type == long_type_node)
    value = build_int_cst (type, ival);
  else if (type == float_type_node || type == double_type_node)
    {
      REAL_VALUE_TYPE x;
      REAL_VALUE_FROM_INT (x, ival, 0, TYPE_MODE (type));
      value = build_real (type, x);
    }
  else
    gcc_unreachable ();

  push_value (value);
}

static void
expand_java_return (tree type)
{
  if (type == void_type_node)
    java_add_stmt (build1 (RETURN_EXPR, void_type_node, NULL));   
  else
    {
      tree retval = pop_value (type);
      tree res = DECL_RESULT (current_function_decl);
      retval = build2 (MODIFY_EXPR, TREE_TYPE (res), res, retval);

      /* Handle the situation where the native integer type is smaller
	 than the JVM integer. It can happen for many cross compilers.
	 The whole if expression just goes away if INT_TYPE_SIZE < 32
	 is false. */
      if (INT_TYPE_SIZE < 32
	  && (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (res)))
	      < GET_MODE_SIZE (TYPE_MODE (type))))
	retval = build1(NOP_EXPR, TREE_TYPE(res), retval);
      
      TREE_SIDE_EFFECTS (retval) = 1;
      java_add_stmt (build1 (RETURN_EXPR, void_type_node, retval));
    }
}

static void
expand_load_internal (int index, tree type, int pc)
{
  tree copy;
  tree var = find_local_variable (index, type, pc);

  /* Now VAR is the VAR_DECL (or PARM_DECL) that we are going to push
     on the stack.  If there is an assignment to this VAR_DECL between
     the stack push and the use, then the wrong code could be
     generated.  To avoid this we create a new local and copy our
     value into it.  Then we push this new local on the stack.
     Hopefully this all gets optimized out.  */
  copy = build_decl (input_location, VAR_DECL, NULL_TREE, type);
  if ((INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type))
      && TREE_TYPE (copy) != TREE_TYPE (var))
    var = convert (type, var);
  java_add_local_var (copy);
  java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (var), copy, var));
  
  push_value (copy);
}

tree
build_address_of (tree value)
{
  return build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (value)), value);
}

bool
class_has_finalize_method (tree type)
{
  tree super = CLASSTYPE_SUPER (type);

  if (super == NULL_TREE)
    return false;	/* Every class with a real finalizer inherits	*/
   			/* from java.lang.Object.			*/
  else
    return HAS_FINALIZER_P (type) || class_has_finalize_method (super);
}

tree
java_create_object (tree type)
{
  tree alloc_node = (class_has_finalize_method (type) 
		     ? alloc_object_node
		     : alloc_no_finalizer_node);
  
  return build_call_nary (promote_type (type),
			  build_address_of (alloc_node),
			  1, build_class_ref (type));
}

static void
expand_java_NEW (tree type)
{
  tree alloc_node;

  alloc_node = (class_has_finalize_method (type) ? alloc_object_node
		  				 : alloc_no_finalizer_node);
  if (! CLASS_LOADED_P (type))
    load_class (type, 1);
  safe_layout_class (type);
  push_value (build_call_nary (promote_type (type),
			       build_address_of (alloc_node),
			       1, build_class_ref (type)));
}

/* This returns an expression which will extract the class of an
   object.  */

tree
build_get_class (tree value)
{
  tree class_field = lookup_field (&dtable_type, get_identifier ("class"));
  tree vtable_field = lookup_field (&object_type_node,
				    get_identifier ("vtable"));
  tree tmp = build3 (COMPONENT_REF, dtable_ptr_type,
		     build_java_indirect_ref (object_type_node, value,
					      flag_check_references),
		     vtable_field, NULL_TREE);
  return build3 (COMPONENT_REF, class_ptr_type,
		 build1 (INDIRECT_REF, dtable_type, tmp),
		 class_field, NULL_TREE);
}

/* This builds the tree representation of the `instanceof' operator.
   It tries various tricks to optimize this in cases where types are
   known.  */

tree
build_instanceof (tree value, tree type)
{
  tree expr;
  tree itype = TREE_TYPE (TREE_TYPE (soft_instanceof_node));
  tree valtype = TREE_TYPE (TREE_TYPE (value));
  tree valclass = TYPE_NAME (valtype);
  tree klass;

  /* When compiling from bytecode, we need to ensure that TYPE has
     been loaded.  */
  if (CLASS_P (type) && ! CLASS_LOADED_P (type))
    {
      load_class (type, 1);
      safe_layout_class (type);
      if (! TYPE_SIZE (type) || TREE_CODE (TYPE_SIZE (type)) == ERROR_MARK)
	return error_mark_node;
    }
  klass = TYPE_NAME (type);

  if (type == object_type_node || inherits_from_p (valtype, type))
    {
      /* Anything except `null' is an instance of Object.  Likewise,
	 if the object is known to be an instance of the class, then
	 we only need to check for `null'.  */
      expr = build2 (NE_EXPR, itype, value, null_pointer_node);
    }
  else if (flag_verify_invocations
	   && ! TYPE_ARRAY_P (type)
	   && ! TYPE_ARRAY_P (valtype)
	   && DECL_P (klass) && DECL_P (valclass)
	   && ! CLASS_INTERFACE (valclass)
	   && ! CLASS_INTERFACE (klass)
	   && ! inherits_from_p (type, valtype)
	   && (CLASS_FINAL (klass)
	       || ! inherits_from_p (valtype, type)))
    {
      /* The classes are from different branches of the derivation
	 tree, so we immediately know the answer.  */
      expr = boolean_false_node;
    }
  else if (DECL_P (klass) && CLASS_FINAL (klass))
    {
      tree save = save_expr (value);
      expr = build3 (COND_EXPR, itype,
		     build2 (NE_EXPR, boolean_type_node,
			     save, null_pointer_node),
		     build2 (EQ_EXPR, itype,
			     build_get_class (save),
			     build_class_ref (type)),
		     boolean_false_node);
    }
  else
    {
      expr = build_call_nary (itype,
			      build_address_of (soft_instanceof_node),
			      2, value, build_class_ref (type));
    }
  TREE_SIDE_EFFECTS (expr) = TREE_SIDE_EFFECTS (value);
  return expr;
}

static void
expand_java_INSTANCEOF (tree type)
{
  tree value = pop_value (object_ptr_type_node);
  value = build_instanceof (value, type);
  push_value (value);
}

static void
expand_java_CHECKCAST (tree type)
{
  tree value = pop_value (ptr_type_node);
  value = build_call_nary (promote_type (type),
			   build_address_of (soft_checkcast_node),
			   2, build_class_ref (type), value);
  push_value (value);
}

static void
expand_iinc (unsigned int local_var_index, int ival, int pc)
{
  tree local_var, res;
  tree constant_value;

  flush_quick_stack ();
  local_var = find_local_variable (local_var_index, int_type_node, pc);
  constant_value = build_int_cst (NULL_TREE, ival);
  res = fold_build2 (PLUS_EXPR, int_type_node, local_var, constant_value);
  java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (local_var), local_var, res));
}


tree
build_java_soft_divmod (enum tree_code op, tree type, tree op1, tree op2)
{
  tree call = NULL;
  tree arg1 = convert (type, op1);
  tree arg2 = convert (type, op2);

  if (type == int_type_node)
    {	  
      switch (op)
	{
	case TRUNC_DIV_EXPR:
	  call = soft_idiv_node;
	  break;
	case TRUNC_MOD_EXPR:
	  call = soft_irem_node;
	  break;
	default:
	  break;
	}
    }
  else if (type == long_type_node)
    {	  
      switch (op)
	{
	case TRUNC_DIV_EXPR:
	  call = soft_ldiv_node;
	  break;
	case TRUNC_MOD_EXPR:
	  call = soft_lrem_node;
	  break;
	default:
	  break;
	}
    }

  gcc_assert (call);
  call = build_call_nary (type, build_address_of (call), 2, arg1, arg2);
  return call;
}

tree
build_java_binop (enum tree_code op, tree type, tree arg1, tree arg2)
{
  tree mask;
  switch (op)
    {
    case URSHIFT_EXPR:
      {
	tree u_type = unsigned_type_for (type);
	arg1 = convert (u_type, arg1);
	arg1 = build_java_binop (RSHIFT_EXPR, u_type, arg1, arg2);
	return convert (type, arg1);
      }
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      mask = build_int_cst (NULL_TREE,
			    TYPE_PRECISION (TREE_TYPE (arg1)) - 1);
      arg2 = fold_build2 (BIT_AND_EXPR, int_type_node, arg2, mask);
      break;

    case COMPARE_L_EXPR:  /* arg1 > arg2 ?  1 : arg1 == arg2 ? 0 : -1 */
    case COMPARE_G_EXPR:  /* arg1 < arg2 ? -1 : arg1 == arg2 ? 0 :  1 */
      arg1 = save_expr (arg1);  arg2 = save_expr (arg2);
      {
	tree ifexp1 = fold_build2 (op == COMPARE_L_EXPR ? GT_EXPR : LT_EXPR,
				   boolean_type_node, arg1, arg2);
	tree ifexp2 = fold_build2 (EQ_EXPR, boolean_type_node, arg1, arg2);
	tree second_compare = fold_build3 (COND_EXPR, int_type_node,
					   ifexp2, integer_zero_node,
					   op == COMPARE_L_EXPR
					   ? integer_minus_one_node
					   : integer_one_node);
	return fold_build3 (COND_EXPR, int_type_node, ifexp1,
			    op == COMPARE_L_EXPR ? integer_one_node
			    : integer_minus_one_node,
			    second_compare);
      }
    case COMPARE_EXPR:
      arg1 = save_expr (arg1);  arg2 = save_expr (arg2);
      {
	tree ifexp1 = fold_build2 (LT_EXPR, boolean_type_node, arg1, arg2);
	tree ifexp2 = fold_build2 (GT_EXPR, boolean_type_node, arg1, arg2);
	tree second_compare = fold_build3 (COND_EXPR, int_type_node,
					   ifexp2, integer_one_node,
					   integer_zero_node);
	return fold_build3 (COND_EXPR, int_type_node,
			    ifexp1, integer_minus_one_node, second_compare);
      }      
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      if (TREE_CODE (type) == REAL_TYPE
	  && op == TRUNC_MOD_EXPR)
	{
	  tree call;
	  if (type != double_type_node)
	    {
	      arg1 = convert (double_type_node, arg1);
	      arg2 = convert (double_type_node, arg2);
	    }
	  call = build_call_nary (double_type_node,
				  build_address_of (soft_fmod_node),
				  2, arg1, arg2);
	  if (type != double_type_node)
	    call = convert (type, call);
	  return call;
	}
      
      if (TREE_CODE (type) == INTEGER_TYPE
	  && flag_use_divide_subroutine
	  && ! flag_syntax_only)
	return build_java_soft_divmod (op, type, arg1, arg2);
      
      break;
    default:  ;
    }
  return fold_build2 (op, type, arg1, arg2);
}

static void
expand_java_binop (tree type, enum tree_code op)
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
   (If the *TYPEP is not found, or if the field reference is
   ambiguous, return error_mark_node.)
   If found, return the FIELD_DECL, and set *TYPEP to the
   class containing the field. */

tree
lookup_field (tree *typep, tree name)
{
  if (CLASS_P (*typep) && !CLASS_LOADED_P (*typep))
    {
      load_class (*typep, 1);
      safe_layout_class (*typep);
      if (!TYPE_SIZE (*typep) || TREE_CODE (TYPE_SIZE (*typep)) == ERROR_MARK)
	return error_mark_node;
    }
  do
    {
      tree field, binfo, base_binfo;
      tree save_field;
      int i;

      for (field = TYPE_FIELDS (*typep); field; field = DECL_CHAIN (field))
	if (DECL_NAME (field) == name)
	  return field;

      /* Process implemented interfaces. */
      save_field = NULL_TREE;
      for (binfo = TYPE_BINFO (*typep), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	{
	  tree t = BINFO_TYPE (base_binfo);
	  if ((field = lookup_field (&t, name)))
	    {
	      if (save_field == field)
		continue;
	      if (save_field == NULL_TREE)
		save_field = field;
	      else
		{
		  tree i1 = DECL_CONTEXT (save_field);
		  tree i2 = DECL_CONTEXT (field);
		  error ("reference %qs is ambiguous: appears in interface %qs and interface %qs",
			 IDENTIFIER_POINTER (name),
			 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (i1))),
			 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (i2))));
		  return error_mark_node;
		}
	    }
	}

      if (save_field != NULL_TREE)
	return save_field;

      *typep = CLASSTYPE_SUPER (*typep);
    } while (*typep);
  return NULL_TREE;
}

/* Look up the field named NAME in object SELF_VALUE,
   which has class SELF_CLASS (a non-handle RECORD_TYPE).
   SELF_VALUE is NULL_TREE if looking for a static field. */

tree
build_field_ref (tree self_value, tree self_class, tree name)
{
  tree base_class = self_class;
  tree field_decl = lookup_field (&base_class, name);
  if (field_decl == NULL_TREE)
    {
      error ("field %qs not found", IDENTIFIER_POINTER (name));
      return error_mark_node;
    }
  if (self_value == NULL_TREE)
    {
      return build_static_field_ref (field_decl);
    }
  else
    {
      tree base_type = promote_type (base_class);

      /* CHECK is true if self_value is not the this pointer.  */
      int check = (! (DECL_P (self_value)
		      && DECL_NAME (self_value) == this_identifier_node));

      /* Determine whether a field offset from NULL will lie within
	 Page 0: this is necessary on those GNU/Linux/BSD systems that
	 trap SEGV to generate NullPointerExceptions.  

	 We assume that Page 0 will be mapped with NOPERM, and that
	 memory may be allocated from any other page, so only field
	 offsets < pagesize are guaranteed to trap.  We also assume
	 the smallest page size we'll encounter is 4k bytes.  */
      if (! flag_syntax_only && check && ! flag_check_references 
	  && ! flag_indirect_dispatch)
	{
	  tree field_offset = byte_position (field_decl);
	  if (! page_size)
	    page_size = size_int (4096); 	      
	  check = ! INT_CST_LT_UNSIGNED (field_offset, page_size);
	}

      if (base_type != TREE_TYPE (self_value))
	self_value = fold_build1 (NOP_EXPR, base_type, self_value);
      if (! flag_syntax_only && flag_indirect_dispatch)
	{
	  tree otable_index
	    = build_int_cst (NULL_TREE, get_symbol_table_index 
			     (field_decl, NULL_TREE, 
			      &TYPE_OTABLE_METHODS (output_class)));
	  tree field_offset
	    = build4 (ARRAY_REF, integer_type_node,
		      TYPE_OTABLE_DECL (output_class), otable_index,
		      NULL_TREE, NULL_TREE);
	  tree address;

	  if (DECL_CONTEXT (field_decl) != output_class)
	    field_offset
	      = build3 (COND_EXPR, TREE_TYPE (field_offset),
			build2 (EQ_EXPR, boolean_type_node,
				field_offset, integer_zero_node),
			build_call_nary (void_type_node, 
					 build_address_of (soft_nosuchfield_node),
					 1, otable_index),
			field_offset);
	  
	  self_value = java_check_reference (self_value, check);
	  address = fold_build_pointer_plus (self_value, field_offset);
	  address = fold_convert (build_pointer_type (TREE_TYPE (field_decl)),
				  address);
	  return fold_build1 (INDIRECT_REF, TREE_TYPE (field_decl), address);
	}

      self_value = build_java_indirect_ref (TREE_TYPE (TREE_TYPE (self_value)),
					    self_value, check);
      return fold_build3 (COMPONENT_REF, TREE_TYPE (field_decl),
			  self_value, field_decl, NULL_TREE);
    }
}

tree
lookup_label (int pc)
{
  tree name;
  char buf[32];
  if (pc > highest_label_pc_this_method)
    highest_label_pc_this_method = pc;
  targetm.asm_out.generate_internal_label (buf, "LJpc=",
					   start_label_pc_this_method + pc);
  name = get_identifier (buf);
  if (IDENTIFIER_LOCAL_VALUE (name))
    return IDENTIFIER_LOCAL_VALUE (name);
  else
    {
      /* The type of the address of a label is return_address_type_node. */
      tree decl = create_label_decl (name);
      return pushdecl (decl);
    }
}

/* Generate a unique name for the purpose of loops and switches
   labels, and try-catch-finally blocks label or temporary variables.  */

tree
generate_name (void)
{
  static int l_number = 0;
  char buff [32];
  targetm.asm_out.generate_internal_label (buff, "LJv", l_number);
  l_number++;
  return get_identifier (buff);
}

tree
create_label_decl (tree name)
{
  tree decl;
  decl = build_decl (input_location, LABEL_DECL, name, 
		     TREE_TYPE (return_address_type_node));
  DECL_CONTEXT (decl) = current_function_decl;
  DECL_IGNORED_P (decl) = 1;
  return decl;
}

/* This maps a bytecode offset (PC) to various flags.  */
char *instruction_bits;

/* This is a vector of type states for the current method.  It is
   indexed by PC.  Each element is a tree vector holding the type
   state at that PC.  We only note type states at basic block
   boundaries.  */
vec<tree, va_gc> *type_states;

static void
note_label (int current_pc ATTRIBUTE_UNUSED, int target_pc)
{
  lookup_label (target_pc);
  instruction_bits [target_pc] |= BCODE_JUMP_TARGET;
}

/* Emit code to jump to TARGET_PC if VALUE1 CONDITION VALUE2,
   where CONDITION is one of one the compare operators. */

static void
expand_compare (enum tree_code condition, tree value1, tree value2,
		int target_pc)
{
  tree target = lookup_label (target_pc);
  tree cond = fold_build2 (condition, boolean_type_node, value1, value2);
  java_add_stmt 
    (build3 (COND_EXPR, void_type_node, java_truthvalue_conversion (cond),
	     build1 (GOTO_EXPR, void_type_node, target), 
	     build_java_empty_stmt ()));
}

/* Emit code for a TEST-type opcode. */

static void
expand_test (enum tree_code condition, tree type, int target_pc)
{
  tree value1, value2;
  flush_quick_stack ();
  value1 = pop_value (type);
  value2 = (type == ptr_type_node) ? null_pointer_node : integer_zero_node;
  expand_compare (condition, value1, value2, target_pc);
}

/* Emit code for a COND-type opcode. */

static void
expand_cond (enum tree_code condition, tree type, int target_pc)
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
expand_java_goto (int target_pc)
{
  tree target_label = lookup_label (target_pc);
  flush_quick_stack ();
  java_add_stmt (build1 (GOTO_EXPR, void_type_node, target_label));
}

static tree
expand_java_switch (tree selector, int default_pc)
{
  tree switch_expr, x;

  flush_quick_stack ();
  switch_expr = build3 (SWITCH_EXPR, TREE_TYPE (selector), selector,
			NULL_TREE, NULL_TREE);
  java_add_stmt (switch_expr);

  x = build_case_label (NULL_TREE, NULL_TREE,
			create_artificial_label (input_location));
  append_to_statement_list (x, &SWITCH_BODY (switch_expr));

  x = build1 (GOTO_EXPR, void_type_node, lookup_label (default_pc));
  append_to_statement_list (x, &SWITCH_BODY (switch_expr));

  return switch_expr;
}

static void
expand_java_add_case (tree switch_expr, int match, int target_pc)
{
  tree value, x;

  value = build_int_cst (TREE_TYPE (switch_expr), match);
  
  x = build_case_label (value, NULL_TREE,
			create_artificial_label (input_location));
  append_to_statement_list (x, &SWITCH_BODY (switch_expr));

  x = build1 (GOTO_EXPR, void_type_node, lookup_label (target_pc));
  append_to_statement_list (x, &SWITCH_BODY (switch_expr));
}

static vec<tree, va_gc> *
pop_arguments (tree method_type)
{
  function_args_iterator fnai;
  tree type;
  vec<tree, va_gc> *args = NULL;
  int arity;

  FOREACH_FUNCTION_ARGS (method_type, type, fnai)
    {
      /* XXX: leaky abstraction.  */
      if (type == void_type_node)
        break;

      vec_safe_push (args, type);
    }

  arity = vec_safe_length (args);

  while (arity--)
    {
      tree arg = pop_value ((*args)[arity]);

      /* We simply cast each argument to its proper type.  This is
	 needed since we lose type information coming out of the
	 verifier.  We also have to do this when we pop an integer
	 type that must be promoted for the function call.  */
      if (TREE_CODE (type) == POINTER_TYPE)
	arg = build1 (NOP_EXPR, type, arg);
      else if (targetm.calls.promote_prototypes (type)
	       && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)
	       && INTEGRAL_TYPE_P (type))
	arg = convert (integer_type_node, arg);

      (*args)[arity] = arg;
    }

  return args;
}

/* Attach to PTR (a block) the declaration found in ENTRY. */

int
attach_init_test_initialization_flags (void **entry, void *ptr)
{
  tree block = (tree)ptr;
  struct treetreehash_entry *ite = (struct treetreehash_entry *) *entry;

  if (block != error_mark_node)
    {
      if (TREE_CODE (block) == BIND_EXPR)
        {
	  tree body = BIND_EXPR_BODY (block);
	  DECL_CHAIN (ite->value) = BIND_EXPR_VARS (block);
	  BIND_EXPR_VARS (block) = ite->value;
	  body = build2 (COMPOUND_EXPR, void_type_node,
			 build1 (DECL_EXPR, void_type_node, ite->value), body);
	  BIND_EXPR_BODY (block) = body;
	}
      else
	{
	  tree body = BLOCK_SUBBLOCKS (block);
	  TREE_CHAIN (ite->value) = BLOCK_EXPR_DECLS (block);
	  BLOCK_EXPR_DECLS (block) = ite->value;
	  body = build2 (COMPOUND_EXPR, void_type_node,
			 build1 (DECL_EXPR, void_type_node, ite->value), body);
	  BLOCK_SUBBLOCKS (block) = body;
        }
      
    }
  return true;
}

/* Build an expression to initialize the class CLAS.
   if EXPR is non-NULL, returns an expression to first call the initializer
   (if it is needed) and then calls EXPR. */

tree
build_class_init (tree clas, tree expr)
{
  tree init;

  /* An optimization: if CLAS is a superclass of the class we're
     compiling, we don't need to initialize it.  However, if CLAS is
     an interface, it won't necessarily be initialized, even if we
     implement it.  */
  if ((! CLASS_INTERFACE (TYPE_NAME (clas))
       && inherits_from_p (current_class, clas))
      || current_class == clas)
    return expr;

  if (always_initialize_class_p)
    {
      init = build_call_nary (void_type_node,
			      build_address_of (soft_initclass_node),
			      1, build_class_ref (clas));
      TREE_SIDE_EFFECTS (init) = 1;
    }
  else
    {
      tree *init_test_decl;
      tree decl;
      init_test_decl = java_treetreehash_new
	(DECL_FUNCTION_INIT_TEST_TABLE (current_function_decl), clas);

      if (*init_test_decl == NULL)
	{
	  /* Build a declaration and mark it as a flag used to track
	     static class initializations. */
	  decl = build_decl (input_location, VAR_DECL, NULL_TREE,
			     boolean_type_node);
	  MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);
	  DECL_CONTEXT (decl) = current_function_decl;
	  DECL_INITIAL (decl) = boolean_false_node;
	  /* Don't emit any symbolic debugging info for this decl.  */
	  DECL_IGNORED_P (decl) = 1;	  
	  *init_test_decl = decl;
	}

      init = build_call_nary (void_type_node,
			      build_address_of (soft_initclass_node),
			      1, build_class_ref (clas));
      TREE_SIDE_EFFECTS (init) = 1;
      init = build3 (COND_EXPR, void_type_node,
		     build2 (EQ_EXPR, boolean_type_node, 
			     *init_test_decl, boolean_false_node),
		     init, integer_zero_node);
      TREE_SIDE_EFFECTS (init) = 1;
      init = build2 (COMPOUND_EXPR, TREE_TYPE (expr), init, 
		     build2 (MODIFY_EXPR, boolean_type_node,
			     *init_test_decl, boolean_true_node));
      TREE_SIDE_EFFECTS (init) = 1;
    }

  if (expr != NULL_TREE)
    {
      expr = build2 (COMPOUND_EXPR, TREE_TYPE (expr), init, expr);
      TREE_SIDE_EFFECTS (expr) = 1;
      return expr;
    }
  return init;
}



/* Rewrite expensive calls that require stack unwinding at runtime to
   cheaper alternatives.  The logic here performs these
   transformations:

   java.lang.Class.forName("foo") -> java.lang.Class.forName("foo", class$)
   java.lang.Class.getClassLoader() -> java.lang.Class.getClassLoader(class$)

*/

typedef struct
{
  const char *classname;
  const char *method;
  const char *signature;
  const char *new_classname;
  const char *new_signature;
  int flags;
  void (*rewrite_arglist) (vec<tree, va_gc> **);
} rewrite_rule;

/* Add __builtin_return_address(0) to the end of an arglist.  */


static void
rewrite_arglist_getcaller (vec<tree, va_gc> **arglist)
{
  tree retaddr 
    = build_call_expr (builtin_decl_explicit (BUILT_IN_RETURN_ADDRESS),
		       1, integer_zero_node);

  DECL_UNINLINABLE (current_function_decl) = 1;

  vec_safe_push (*arglist, retaddr);
}

/* Add this.class to the end of an arglist.  */

static void
rewrite_arglist_getclass (vec<tree, va_gc> **arglist)
{
  vec_safe_push (*arglist, build_class_ref (output_class));
}

static rewrite_rule rules[] =
  {{"java.lang.Class", "getClassLoader", "()Ljava/lang/ClassLoader;", 
    "java.lang.Class", "(Ljava/lang/Class;)Ljava/lang/ClassLoader;", 
    ACC_FINAL|ACC_PRIVATE, rewrite_arglist_getclass},

   {"java.lang.Class", "forName", "(Ljava/lang/String;)Ljava/lang/Class;",
    "java.lang.Class", "(Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/Class;",
    ACC_FINAL|ACC_PRIVATE|ACC_STATIC, rewrite_arglist_getclass},

   {"gnu.classpath.VMStackWalker", "getCallingClass", "()Ljava/lang/Class;",
    "gnu.classpath.VMStackWalker", "(Lgnu/gcj/RawData;)Ljava/lang/Class;",
    ACC_FINAL|ACC_PRIVATE|ACC_STATIC, rewrite_arglist_getcaller},

   {"gnu.classpath.VMStackWalker", "getCallingClassLoader", 
    "()Ljava/lang/ClassLoader;",
    "gnu.classpath.VMStackWalker", "(Lgnu/gcj/RawData;)Ljava/lang/ClassLoader;",
    ACC_FINAL|ACC_PRIVATE|ACC_STATIC, rewrite_arglist_getcaller},

   {"gnu.java.lang.VMCPStringBuilder", "toString", "([CII)Ljava/lang/String;", 
    "java.lang.String", "([CII)Ljava/lang/String;",
    ACC_FINAL|ACC_PRIVATE|ACC_STATIC, NULL},

   {NULL, NULL, NULL, NULL, NULL, 0, NULL}};

/* True if this method is special, i.e. it's a private method that
   should be exported from a DSO.  */

bool
special_method_p (tree candidate_method)
{
  tree context = DECL_NAME (TYPE_NAME (DECL_CONTEXT (candidate_method)));
  tree method = DECL_NAME (candidate_method);
  rewrite_rule *p;

  for (p = rules; p->classname; p++)
    {
      if (get_identifier (p->classname) == context
	  && get_identifier (p->method) == method)
	return true;
    }
  return false;
}

/* Scan the rules list for replacements for *METHOD_P and replace the
   args accordingly.  If the rewrite results in an access to a private
   method, update SPECIAL.*/

void
maybe_rewrite_invocation (tree *method_p, vec<tree, va_gc> **arg_list_p, 
			  tree *method_signature_p, tree *special)
{
  tree context = DECL_NAME (TYPE_NAME (DECL_CONTEXT (*method_p)));
  rewrite_rule *p;
  *special = NULL_TREE;

  for (p = rules; p->classname; p++)
    {
      if (get_identifier (p->classname) == context)
	{
	  tree method = DECL_NAME (*method_p);
	  if (get_identifier (p->method) == method
	      && get_identifier (p->signature) == *method_signature_p)
	    {
	      tree maybe_method;
	      tree destination_class 
		= lookup_class (get_identifier (p->new_classname));
	      gcc_assert (destination_class);
	      maybe_method
		= lookup_java_method (destination_class,
				      method,
				      get_identifier (p->new_signature));
	      if (! maybe_method && ! flag_verify_invocations)
		{
		  maybe_method
		    = add_method (destination_class, p->flags, 
				  method, get_identifier (p->new_signature));
		  DECL_EXTERNAL (maybe_method) = 1;
		}
	      *method_p = maybe_method;
	      gcc_assert (*method_p);
	      if (p->rewrite_arglist)
		p->rewrite_arglist (arg_list_p);
	      *method_signature_p = get_identifier (p->new_signature);
	      *special = integer_one_node;

	      break;
	    }
	}
    }
}



tree
build_known_method_ref (tree method, tree method_type ATTRIBUTE_UNUSED,
			tree self_type, tree method_signature ATTRIBUTE_UNUSED,
			vec<tree, va_gc> *arg_list ATTRIBUTE_UNUSED, tree special)
{
  tree func;
  if (is_compiled_class (self_type))
    {
      /* With indirect dispatch we have to use indirect calls for all
	 publicly visible methods or gcc will use PLT indirections
	 to reach them.  We also have to use indirect dispatch for all
	 external methods.  */
      if (! flag_indirect_dispatch 
	  || (! DECL_EXTERNAL (method) && ! TREE_PUBLIC (method)))
	{
	  func = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (method)),
			 method);
	}
      else
	{
	  tree table_index
	    = build_int_cst (NULL_TREE, 
			     (get_symbol_table_index 
			      (method, special,
			       &TYPE_ATABLE_METHODS (output_class))));
	  func 
	    = build4 (ARRAY_REF,  
		      TREE_TYPE (TREE_TYPE (TYPE_ATABLE_DECL (output_class))),
		      TYPE_ATABLE_DECL (output_class), table_index,
		      NULL_TREE, NULL_TREE);
	}
      func = convert (method_ptr_type_node, func);
    }
  else
    {
      /* We don't know whether the method has been (statically) compiled.
	 Compile this code to get a reference to the method's code:

	 SELF_TYPE->methods[METHOD_INDEX].ncode

      */

      int method_index = 0;
      tree meth, ref;

      /* The method might actually be declared in some superclass, so
	 we have to use its class context, not the caller's notion of
	 where the method is.  */
      self_type = DECL_CONTEXT (method);
      ref = build_class_ref (self_type);
      ref = build1 (INDIRECT_REF, class_type_node, ref);
      if (ncode_ident == NULL_TREE)
	ncode_ident = get_identifier ("ncode");
      if (methods_ident == NULL_TREE)
	methods_ident = get_identifier ("methods");
      ref = build3 (COMPONENT_REF, method_ptr_type_node, ref,
		    lookup_field (&class_type_node, methods_ident),
		    NULL_TREE);
      for (meth = TYPE_METHODS (self_type);
	   ; meth = DECL_CHAIN (meth))
	{
	  if (method == meth)
	    break;
	  if (meth == NULL_TREE)
	    fatal_error ("method '%s' not found in class",
			 IDENTIFIER_POINTER (DECL_NAME (method)));
	  method_index++;
	}
      method_index *= int_size_in_bytes (method_type_node);
      ref = fold_build_pointer_plus_hwi (ref, method_index);
      ref = build1 (INDIRECT_REF, method_type_node, ref);
      func = build3 (COMPONENT_REF, nativecode_ptr_type_node,
		     ref, lookup_field (&method_type_node, ncode_ident),
		     NULL_TREE);
    }
  return func;
}

tree
invoke_build_dtable (int is_invoke_interface, vec<tree, va_gc> *arg_list)
{
  tree dtable, objectref;
  tree saved = save_expr ((*arg_list)[0]);

  (*arg_list)[0] = saved;

  /* If we're dealing with interfaces and if the objectref
     argument is an array then get the dispatch table of the class
     Object rather than the one from the objectref.  */
  objectref = (is_invoke_interface 
	       && is_array_type_p (TREE_TYPE (saved))
	       ? build_class_ref (object_type_node) : saved);

  if (dtable_ident == NULL_TREE)
    dtable_ident = get_identifier ("vtable");
  dtable = build_java_indirect_ref (object_type_node, objectref, 
				    flag_check_references);
  dtable = build3 (COMPONENT_REF, dtable_ptr_type, dtable,
		   lookup_field (&object_type_node, dtable_ident), NULL_TREE);

  return dtable;
}

/* Determine the index in SYMBOL_TABLE for a reference to the decl
   T. If this decl has not been seen before, it will be added to the
   [oa]table_methods. If it has, the existing table slot will be
   reused.  */

int
get_symbol_table_index (tree t, tree special,
			vec<method_entry, va_gc> **symbol_table)
{
  method_entry *e;
  unsigned i;
  method_entry elem = {t, special};

  FOR_EACH_VEC_SAFE_ELT (*symbol_table, i, e)
    if (t == e->method && special == e->special)
      goto done;

  vec_safe_push (*symbol_table, elem);

 done:
  return i + 1;
}

tree 
build_invokevirtual (tree dtable, tree method, tree special)
{
  tree func;
  tree nativecode_ptr_ptr_type_node
    = build_pointer_type (nativecode_ptr_type_node);
  tree method_index;
  tree otable_index;

  if (flag_indirect_dispatch)
    {
      gcc_assert (! CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (method))));

      otable_index 
	= build_int_cst (NULL_TREE, get_symbol_table_index 
			 (method, special,
			  &TYPE_OTABLE_METHODS (output_class)));
      method_index = build4 (ARRAY_REF, integer_type_node, 
			     TYPE_OTABLE_DECL (output_class), 
			     otable_index, NULL_TREE, NULL_TREE);
    }
  else
    {
      /* We fetch the DECL_VINDEX field directly here, rather than
	 using get_method_index().  DECL_VINDEX is the true offset
	 from the vtable base to a method, regrdless of any extra
	 words inserted at the start of the vtable.  */
      method_index = DECL_VINDEX (method);
      method_index = size_binop (MULT_EXPR, method_index,
				 TYPE_SIZE_UNIT (nativecode_ptr_ptr_type_node));
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	method_index = size_binop (MULT_EXPR, method_index,
				   size_int (TARGET_VTABLE_USES_DESCRIPTORS));
    }

  func = fold_build_pointer_plus (dtable, method_index);

  if (TARGET_VTABLE_USES_DESCRIPTORS)
    func = build1 (NOP_EXPR, nativecode_ptr_type_node, func);
  else
    {
      func = fold_convert (nativecode_ptr_ptr_type_node, func);
      func = build1 (INDIRECT_REF, nativecode_ptr_type_node, func);
    }

  return func;
}

static GTY(()) tree class_ident;
tree
build_invokeinterface (tree dtable, tree method)
{
  tree interface;
  tree idx;

  /* We expand invokeinterface here.  */
	    
  if (class_ident == NULL_TREE)
    class_ident = get_identifier ("class");

  dtable = build_java_indirect_ref (dtable_type, dtable,
				    flag_check_references);
  dtable = build3 (COMPONENT_REF, class_ptr_type, dtable,
		   lookup_field (&dtable_type, class_ident), NULL_TREE);

  interface = DECL_CONTEXT (method);
  gcc_assert (CLASS_INTERFACE (TYPE_NAME (interface)));
  layout_class_methods (interface);
  
  if (flag_indirect_dispatch)
    {
      int itable_index 
	= 2 * (get_symbol_table_index 
	       (method, NULL_TREE, &TYPE_ITABLE_METHODS (output_class)));
      interface 
	= build4 (ARRAY_REF, 
		 TREE_TYPE (TREE_TYPE (TYPE_ITABLE_DECL (output_class))),
		 TYPE_ITABLE_DECL (output_class), 
		  build_int_cst (NULL_TREE, itable_index-1),
		  NULL_TREE, NULL_TREE);
      idx 
	= build4 (ARRAY_REF, 
		 TREE_TYPE (TREE_TYPE (TYPE_ITABLE_DECL (output_class))),
		 TYPE_ITABLE_DECL (output_class), 
		  build_int_cst (NULL_TREE, itable_index),
		  NULL_TREE, NULL_TREE);
      interface = convert (class_ptr_type, interface);
      idx = convert (integer_type_node, idx);
    }
  else
    {
      idx = build_int_cst (NULL_TREE, 
			   get_interface_method_index (method, interface));
      interface = build_class_ref (interface);
    }
				     			  
  return build_call_nary (ptr_type_node, 
			  build_address_of (soft_lookupinterfacemethod_node),
			  3, dtable, interface, idx);
}
  
/* Expand one of the invoke_* opcodes.
   OPCODE is the specific opcode.
   METHOD_REF_INDEX is an index into the constant pool.
   NARGS is the number of arguments, or -1 if not specified. */

static void
expand_invoke (int opcode, int method_ref_index, int nargs ATTRIBUTE_UNUSED)
{
  tree method_signature
    = COMPONENT_REF_SIGNATURE(&current_jcf->cpool, method_ref_index);
  tree method_name = COMPONENT_REF_NAME (&current_jcf->cpool,
					 method_ref_index);
  tree self_type
    = get_class_constant (current_jcf,
                          COMPONENT_REF_CLASS_INDEX(&current_jcf->cpool,
                          method_ref_index));
  const char *const self_name
    = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (self_type)));
  tree call, func, method, method_type;
  vec<tree, va_gc> *arg_list;
  tree check = NULL_TREE;

  tree special = NULL_TREE;

  if (! CLASS_LOADED_P (self_type))
    {
      load_class (self_type, 1);
      safe_layout_class (self_type);
      if (TREE_CODE (TYPE_SIZE (self_type)) == ERROR_MARK)
	fatal_error ("failed to find class '%s'", self_name);
    }
  layout_class_methods (self_type);

  if (ID_INIT_P (method_name))
    method = lookup_java_constructor (self_type, method_signature);
  else
    method = lookup_java_method (self_type, method_name, method_signature);

  /* We've found a method in a class other than the one in which it
     was wanted.  This can happen if, for instance, we're trying to
     compile invokespecial super.equals().  
     FIXME: This is a kludge.  Rather than nullifying the result, we
     should change lookup_java_method() so that it doesn't search the
     superclass chain when we're BC-compiling.  */
  if (! flag_verify_invocations
      && method
      && ! TYPE_ARRAY_P (self_type)
      && self_type != DECL_CONTEXT (method))
    method = NULL_TREE;

  /* We've found a method in an interface, but this isn't an interface
     call.  */
  if (opcode != OPCODE_invokeinterface
      && method
      && (CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (method)))))
    method = NULL_TREE;

  /* We've found a non-interface method but we are making an
     interface call.  This can happen if the interface overrides a
     method in Object.  */
  if (! flag_verify_invocations
      && opcode == OPCODE_invokeinterface
      && method
      && ! CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (method))))
    method = NULL_TREE;

  if (method == NULL_TREE)
    {
      if (flag_verify_invocations || ! flag_indirect_dispatch)
	{
	  error ("class '%s' has no method named '%s' matching signature '%s'",
		 self_name,
		 IDENTIFIER_POINTER (method_name),
		 IDENTIFIER_POINTER (method_signature));
	}
      else
	{
	  int flags = ACC_PUBLIC;
	  if (opcode == OPCODE_invokestatic)
	    flags |= ACC_STATIC;
	  if (opcode == OPCODE_invokeinterface)
	    {
	      flags |= ACC_INTERFACE | ACC_ABSTRACT;
	      CLASS_INTERFACE (TYPE_NAME (self_type)) = 1;
	    }
	  method = add_method (self_type, flags, method_name,
			       method_signature);
	  DECL_ARTIFICIAL (method) = 1;
	  METHOD_DUMMY (method) = 1;
	  layout_class_method (self_type, NULL,
			       method, NULL);
	}
    }

  /* Invoke static can't invoke static/abstract method */
  if (method != NULL_TREE)
    {
      if (opcode == OPCODE_invokestatic)
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
    }

  if (method == NULL_TREE)
    {
      /* If we got here, we emitted an error message above.  So we
	 just pop the arguments, push a properly-typed zero, and
	 continue.  */
      method_type = get_type_from_signature (method_signature);
      pop_arguments (method_type);
      if (opcode != OPCODE_invokestatic) 
	pop_type (self_type);
      method_type = promote_type (TREE_TYPE (method_type));
      push_value (convert (method_type, integer_zero_node));
      return;
    }

  arg_list = pop_arguments (TREE_TYPE (method));
  flush_quick_stack ();

  maybe_rewrite_invocation (&method, &arg_list, &method_signature,
			    &special);
  method_type = TREE_TYPE (method);

  func = NULL_TREE;
  if (opcode == OPCODE_invokestatic)
    func = build_known_method_ref (method, method_type, self_type,
				   method_signature, arg_list, special);
  else if (opcode == OPCODE_invokespecial
	   || (opcode == OPCODE_invokevirtual
	       && (METHOD_PRIVATE (method)
		   || METHOD_FINAL (method) 
		   || CLASS_FINAL (TYPE_NAME (self_type)))))
    {
      /* If the object for the method call is null, we throw an
	 exception.  We don't do this if the object is the current
	 method's `this'.  In other cases we just rely on an
	 optimization pass to eliminate redundant checks.  FIXME:
	 Unfortunately there doesn't seem to be a way to determine
	 what the current method is right now.
	 We do omit the check if we're calling <init>.  */
      /* We use a SAVE_EXPR here to make sure we only evaluate
	 the new `self' expression once.  */
      tree save_arg = save_expr ((*arg_list)[0]);
      (*arg_list)[0] = save_arg;
      check = java_check_reference (save_arg, ! DECL_INIT_P (method));
      func = build_known_method_ref (method, method_type, self_type,
				     method_signature, arg_list, special);
    }
  else
    {
      tree dtable = invoke_build_dtable (opcode == OPCODE_invokeinterface, 
					 arg_list);
      if (opcode == OPCODE_invokevirtual)
	func = build_invokevirtual (dtable, method, special);
      else
	func = build_invokeinterface (dtable, method);
    }
      
  if (TREE_CODE (func) == ADDR_EXPR)
    TREE_TYPE (func) = build_pointer_type (method_type);
  else
    func = build1 (NOP_EXPR, build_pointer_type (method_type), func);

  call = build_call_vec (TREE_TYPE (method_type), func, arg_list);
  TREE_SIDE_EFFECTS (call) = 1;
  call = check_for_builtin (method, call);

  if (check != NULL_TREE)
    {
      call = build2 (COMPOUND_EXPR, TREE_TYPE (call), check, call);
      TREE_SIDE_EFFECTS (call) = 1;
    }

  if (TREE_CODE (TREE_TYPE (method_type)) == VOID_TYPE)
    java_add_stmt (call);
  else
    {
      push_value (call);
      flush_quick_stack ();
    }
}

/* Create a stub which will be put into the vtable but which will call
   a JNI function.  */

tree
build_jni_stub (tree method)
{
  tree jnifunc, call, body, method_sig, arg_types;
  tree jniarg0, jniarg1, jniarg2, jniarg3;
  tree jni_func_type, tem;
  tree env_var, res_var = NULL_TREE, block;
  tree method_args;
  tree meth_var;
  tree bind;
  vec<tree, va_gc> *args = NULL;
  int args_size = 0;

  tree klass = DECL_CONTEXT (method);
  klass = build_class_ref (klass);

  gcc_assert (METHOD_NATIVE (method) && flag_jni);

  DECL_ARTIFICIAL (method) = 1;
  DECL_EXTERNAL (method) = 0;

  env_var = build_decl (input_location,
			VAR_DECL, get_identifier ("env"), ptr_type_node);
  DECL_CONTEXT (env_var) = method;

  if (TREE_TYPE (TREE_TYPE (method)) != void_type_node)
    {
      res_var = build_decl (input_location, VAR_DECL, get_identifier ("res"),
			    TREE_TYPE (TREE_TYPE (method)));
      DECL_CONTEXT (res_var) = method;
      DECL_CHAIN (env_var) = res_var;
    }

  method_args = DECL_ARGUMENTS (method);
  block = build_block (env_var, NULL_TREE, method_args, NULL_TREE);
  TREE_SIDE_EFFECTS (block) = 1;

  /* Compute the local `env' by calling _Jv_GetJNIEnvNewFrame.  */
  body = build2 (MODIFY_EXPR, ptr_type_node, env_var,
		 build_call_nary (ptr_type_node,
				  build_address_of (soft_getjnienvnewframe_node),
				  1, klass));

  /* The JNIEnv structure is the first argument to the JNI function.  */
  args_size += int_size_in_bytes (TREE_TYPE (env_var));
  vec_safe_push (args, env_var);

  /* For a static method the second argument is the class.  For a
     non-static method the second argument is `this'; that is already
     available in the argument list.  */
  if (METHOD_STATIC (method))
    {
      args_size += int_size_in_bytes (TREE_TYPE (klass));
      vec_safe_push (args, klass);
    }

  /* All the arguments to this method become arguments to the
     underlying JNI function.  If we had to wrap object arguments in a
     special way, we would do that here.  */
  for (tem = method_args; tem != NULL_TREE; tem = DECL_CHAIN (tem))
    {
      int arg_bits = TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tem)));
#ifdef PARM_BOUNDARY
      arg_bits = (((arg_bits + PARM_BOUNDARY - 1) / PARM_BOUNDARY)
                  * PARM_BOUNDARY);
#endif
      args_size += (arg_bits / BITS_PER_UNIT);

      vec_safe_push (args, tem);
    }
  arg_types = TYPE_ARG_TYPES (TREE_TYPE (method));

  /* Argument types for static methods and the JNIEnv structure.
     FIXME: Write and use build_function_type_vec to avoid this.  */
  if (METHOD_STATIC (method))
    arg_types = tree_cons (NULL_TREE, object_ptr_type_node, arg_types);
  arg_types = tree_cons (NULL_TREE, ptr_type_node, arg_types);

  /* We call _Jv_LookupJNIMethod to find the actual underlying
     function pointer.  _Jv_LookupJNIMethod will throw the appropriate
     exception if this function is not found at runtime.  */
  method_sig = build_java_signature (TREE_TYPE (method));
  jniarg0 = klass;
  jniarg1 = build_utf8_ref (DECL_NAME (method));
  jniarg2 = build_utf8_ref (unmangle_classname
			    (IDENTIFIER_POINTER (method_sig),
			     IDENTIFIER_LENGTH (method_sig)));
  jniarg3 = build_int_cst (NULL_TREE, args_size);

  tem = build_function_type (TREE_TYPE (TREE_TYPE (method)), arg_types);

#ifdef MODIFY_JNI_METHOD_CALL
  tem = MODIFY_JNI_METHOD_CALL (tem);
#endif

  jni_func_type = build_pointer_type (tem);

  /* Use the actual function type, rather than a generic pointer type,
     such that this decl keeps the actual pointer type from being
     garbage-collected.  If it is, we end up using canonical types
     with different uids for equivalent function types, and this in
     turn causes utf8 identifiers and output order to vary.  */
  meth_var = build_decl (input_location,
			 VAR_DECL, get_identifier ("meth"), jni_func_type);
  TREE_STATIC (meth_var) = 1;
  TREE_PUBLIC (meth_var) = 0;
  DECL_EXTERNAL (meth_var) = 0;
  DECL_CONTEXT (meth_var) = method;
  DECL_ARTIFICIAL (meth_var) = 1;
  DECL_INITIAL (meth_var) = null_pointer_node;
  TREE_USED (meth_var) = 1;
  chainon (env_var, meth_var);
  build_result_decl (method);

  jnifunc = build3 (COND_EXPR, jni_func_type,
		    build2 (NE_EXPR, boolean_type_node,
			    meth_var, build_int_cst (TREE_TYPE (meth_var), 0)),
		    meth_var,
		    build2 (MODIFY_EXPR, jni_func_type, meth_var,
			    build1
			    (NOP_EXPR, jni_func_type,
			     build_call_nary (ptr_type_node,
					      build_address_of
					      (soft_lookupjnimethod_node),
					      4,
					      jniarg0, jniarg1,
					      jniarg2, jniarg3))));

  /* Now we make the actual JNI call via the resulting function
     pointer.    */
  call = build_call_vec (TREE_TYPE (TREE_TYPE (method)), jnifunc, args);

  /* If the JNI call returned a result, capture it here.  If we had to
     unwrap JNI object results, we would do that here.  */
  if (res_var != NULL_TREE)
    {
      /* If the call returns an object, it may return a JNI weak
	 reference, in which case we must unwrap it.  */
      if (! JPRIMITIVE_TYPE_P (TREE_TYPE (TREE_TYPE (method))))
	call = build_call_nary (TREE_TYPE (TREE_TYPE (method)),
				build_address_of (soft_unwrapjni_node),
				1, call);
      call = build2 (MODIFY_EXPR, TREE_TYPE (TREE_TYPE (method)),
		     res_var, call);
    }

  TREE_SIDE_EFFECTS (call) = 1;

  body = build2 (COMPOUND_EXPR, void_type_node, body, call);
  TREE_SIDE_EFFECTS (body) = 1;

  /* Now free the environment we allocated.  */
  call = build_call_nary (ptr_type_node,
			  build_address_of (soft_jnipopsystemframe_node),
			  1, env_var);
  TREE_SIDE_EFFECTS (call) = 1;
  body = build2 (COMPOUND_EXPR, void_type_node, body, call);
  TREE_SIDE_EFFECTS (body) = 1;

  /* Finally, do the return.  */
  if (res_var != NULL_TREE)
    {
      tree drt;
      gcc_assert (DECL_RESULT (method));
      /* Make sure we copy the result variable to the actual
	 result.  We use the type of the DECL_RESULT because it
	 might be different from the return type of the function:
	 it might be promoted.  */
      drt = TREE_TYPE (DECL_RESULT (method));
      if (drt != TREE_TYPE (res_var))
	res_var = build1 (CONVERT_EXPR, drt, res_var);
      res_var = build2 (MODIFY_EXPR, drt, DECL_RESULT (method), res_var);
      TREE_SIDE_EFFECTS (res_var) = 1;
    }

  body = build2 (COMPOUND_EXPR, void_type_node, body,
		 build1 (RETURN_EXPR, void_type_node, res_var));
  TREE_SIDE_EFFECTS (body) = 1;
  
  /* Prepend class initialization for static methods reachable from
     other classes.  */
  if (METHOD_STATIC (method)
      && (! METHOD_PRIVATE (method)
          || INNER_CLASS_P (DECL_CONTEXT (method))))
    {
      tree init = build_call_expr (soft_initclass_node, 1, 
				   klass);
      body = build2 (COMPOUND_EXPR, void_type_node, init, body);
      TREE_SIDE_EFFECTS (body) = 1;
    }

  bind = build3 (BIND_EXPR, void_type_node, BLOCK_VARS (block), 
		 body, block);
  return bind;
}


/* Given lvalue EXP, return a volatile expression that references the
   same object.  */

tree
java_modify_addr_for_volatile (tree exp)
{
  tree exp_type = TREE_TYPE (exp);
  tree v_type 
    = build_qualified_type (exp_type,
			    TYPE_QUALS (exp_type) | TYPE_QUAL_VOLATILE);
  tree addr = build_fold_addr_expr (exp);
  v_type = build_pointer_type (v_type);
  addr = fold_convert (v_type, addr);
  exp = build_fold_indirect_ref (addr);
  return exp;
}


/* Expand an operation to extract from or store into a field.
   IS_STATIC is 1 iff the field is static.
   IS_PUTTING is 1 for putting into a field;  0 for getting from the field.
   FIELD_REF_INDEX is an index into the constant pool.  */

static void
expand_java_field_op (int is_static, int is_putting, int field_ref_index)
{
  tree self_type
    = get_class_constant (current_jcf,
                          COMPONENT_REF_CLASS_INDEX (&current_jcf->cpool,
                          field_ref_index));
  const char *self_name
    = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (self_type)));
  tree field_name = COMPONENT_REF_NAME (&current_jcf->cpool, field_ref_index);
  tree field_signature = COMPONENT_REF_SIGNATURE (&current_jcf->cpool, 
						  field_ref_index);
  tree field_type = get_type_from_signature (field_signature);
  tree new_value = is_putting ? pop_value (field_type) : NULL_TREE;
  tree field_ref;
  int is_error = 0;
  tree original_self_type = self_type;
  tree field_decl;
  tree modify_expr;
  
  if (! CLASS_LOADED_P (self_type))
    load_class (self_type, 1);  
  field_decl = lookup_field (&self_type, field_name);
  if (field_decl == error_mark_node)
    {
      is_error = 1;
    }
  else if (field_decl == NULL_TREE)
    {
      if (! flag_verify_invocations)
	{
	  int flags = ACC_PUBLIC;
	  if (is_static)
	    flags |= ACC_STATIC;
	  self_type = original_self_type;
	  field_decl = add_field (original_self_type, field_name,
				  field_type, flags); 
	  DECL_ARTIFICIAL (field_decl) = 1;
	  DECL_IGNORED_P (field_decl) = 1;
#if 0
	  /* FIXME: We should be pessimistic about volatility.  We
	     don't know one way or another, but this is safe.
	     However, doing this has bad effects on code quality.  We
	     need to look at better ways to do this.  */
	  TREE_THIS_VOLATILE (field_decl) = 1;
#endif
	}
      else
	{      
	  error ("missing field '%s' in '%s'",
		 IDENTIFIER_POINTER (field_name), self_name);
	  is_error = 1;
      }
    }
  else if (build_java_signature (TREE_TYPE (field_decl)) != field_signature)
    {
      error ("mismatching signature for field '%s' in '%s'",
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

  field_ref = build_field_ref (field_ref, self_type, field_name);
  if (is_static
      && ! flag_indirect_dispatch)
    {
      tree context = DECL_CONTEXT (field_ref);
      if (context != self_type && CLASS_INTERFACE (TYPE_NAME (context)))
	field_ref = build_class_init (context, field_ref);
      else
	field_ref = build_class_init (self_type, field_ref);
    }
  if (is_putting)
    {
      flush_quick_stack ();
      if (FIELD_FINAL (field_decl))
	{
	  if (DECL_CONTEXT (field_decl) != current_class)
            error ("assignment to final field %q+D not in field%'s class",
                   field_decl);
	  /* We used to check for assignments to final fields not
	     occurring in the class initializer or in a constructor
	     here.  However, this constraint doesn't seem to be
	     enforced by the JVM.  */
	}      

      if (TREE_THIS_VOLATILE (field_decl))
	field_ref = java_modify_addr_for_volatile (field_ref);

      modify_expr = build2 (MODIFY_EXPR, TREE_TYPE (field_ref),
			    field_ref, new_value);

      if (TREE_THIS_VOLATILE (field_decl))
	{
	  tree sync = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
	  java_add_stmt (build_call_expr (sync, 0));
	}
      	  
      java_add_stmt (modify_expr);
    }
  else
    {
      tree temp = build_decl (input_location,
			      VAR_DECL, NULL_TREE, TREE_TYPE (field_ref));
      java_add_local_var (temp);

      if (TREE_THIS_VOLATILE (field_decl))
	field_ref = java_modify_addr_for_volatile (field_ref);

      modify_expr 
	= build2 (MODIFY_EXPR, TREE_TYPE (field_ref), temp, field_ref);
      java_add_stmt (modify_expr);

      if (TREE_THIS_VOLATILE (field_decl))
	{
	  tree sync = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
	  java_add_stmt (build_call_expr (sync, 0));
	}

      push_value (temp);
    }      
  TREE_THIS_VOLATILE (field_ref) = TREE_THIS_VOLATILE (field_decl);
}

static void
load_type_state (int pc)
{
  int i;
  tree vec = (*type_states)[pc];
  int cur_length = TREE_VEC_LENGTH (vec);
  stack_pointer = cur_length - DECL_MAX_LOCALS(current_function_decl);
  for (i = 0; i < cur_length; i++)
    type_map [i] = TREE_VEC_ELT (vec, i);
}

/* Go over METHOD's bytecode and note instruction starts in
   instruction_bits[].  */

void
note_instructions (JCF *jcf, tree method)
{
  int PC; 
  unsigned char* byte_ops;
  long length = DECL_CODE_LENGTH (method);

  int saw_index;
  jint INT_temp;

#undef RET /* Defined by config/i386/i386.h */
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
#define CONST_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define CONST_INDEX_2 (saw_index = 1, IMMEDIATE_u2)
#define VAR_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define VAR_INDEX_2 (saw_index = 1, IMMEDIATE_u2)

#define CHECK_PC_IN_RANGE(PC) ((void)1) /* Already handled by verifier. */

  JCF_SEEK (jcf, DECL_CODE_OFFSET (method));
  byte_ops = jcf->read_ptr;
  instruction_bits = XRESIZEVAR (char, instruction_bits, length + 1);
  memset (instruction_bits, 0, length + 1);
  vec_alloc (type_states, length + 1);
  type_states->quick_grow_cleared (length + 1);

  /* This pass figures out which PC can be the targets of jumps. */
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
  NOTE_LABEL (PC); \
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
}

void
expand_byte_code (JCF *jcf, tree method)
{
  int PC;
  int i;
  const unsigned char *linenumber_pointer;
  int dead_code_index = -1;
  unsigned char* byte_ops;
  long length = DECL_CODE_LENGTH (method);
  location_t max_location = input_location;

  stack_pointer = 0;
  JCF_SEEK (jcf, DECL_CODE_OFFSET (method));
  byte_ops = jcf->read_ptr;

  /* We make an initial pass of the line number table, to note
     which instructions have associated line number entries. */
  linenumber_pointer = linenumber_table;
  for (i = 0; i < linenumber_count; i++)
    {
      int pc = GET_u2 (linenumber_pointer);
      linenumber_pointer += 4;
      if (pc >= length)
	warning (0, "invalid PC in line number table");
      else
	{
	  if ((instruction_bits[pc] & BCODE_HAS_LINENUMBER) != 0)
	    instruction_bits[pc] |= BCODE_HAS_MULTI_LINENUMBERS;
	  instruction_bits[pc] |= BCODE_HAS_LINENUMBER;
	}
    }  

  if (! verify_jvm_instructions_new (jcf, byte_ops, length))
    return;

  promote_arguments ();
  cache_this_class_ref (method);
  cache_cpool_data_ref ();

  /* Translate bytecodes.  */
  linenumber_pointer = linenumber_table;
  for (PC = 0; PC < length;)
    {
      if ((instruction_bits [PC] & BCODE_TARGET) != 0 || PC == 0)
	{
	  tree label = lookup_label (PC);
          flush_quick_stack ();
	  if ((instruction_bits [PC] & BCODE_TARGET) != 0)
	    java_add_stmt (build1 (LABEL_EXPR, void_type_node, label));
	  if ((instruction_bits[PC] & BCODE_VERIFIED) != 0)
	    load_type_state (PC);
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
	      if (extra_warnings)
		warning (0, "unreachable bytecode from %d to before %d",
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
		  int line = GET_u2 (linenumber_pointer - 2);
		  input_location = linemap_line_start (line_table, line, 1);
		  if (input_location > max_location)
		    max_location = input_location;
		  if (!(instruction_bits[PC] & BCODE_HAS_MULTI_LINENUMBERS))
		    break;
		}
	    }
	}
      maybe_pushlevels (PC);
      PC = process_jvm_instruction (PC, byte_ops, length);
      maybe_poplevels (PC);
    } /* for */

  uncache_this_class_ref (method);

  if (dead_code_index != -1)
    {
      /* We've just reached the end of a region of dead code.  */
      if (extra_warnings)
	warning (0, "unreachable bytecode from %d to the end of the method", 
		 dead_code_index);
    }

  DECL_FUNCTION_LAST_LINE (method) = max_location;
}

static void
java_push_constant_from_pool (JCF *jcf, int index)
{
  tree c;
  if (JPOOL_TAG (jcf, index) == CONSTANT_String)
    {
      tree name;
      name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index));
      index = alloc_name_constant (CONSTANT_String, name);
      c = build_ref_from_constant_pool (index);
      c = convert (promote_type (string_type_node), c);
    }
  else if (JPOOL_TAG (jcf, index) == CONSTANT_Class
	   || JPOOL_TAG (jcf, index) == CONSTANT_ResolvedClass)
    {
      tree record = get_class_constant (jcf, index);
      c = build_class_ref (record);
    }
  else
    c = get_constant (jcf, index);
  push_value (c);
} 

int
process_jvm_instruction (int PC, const unsigned char* byte_ops,
			 long length ATTRIBUTE_UNUSED)
{ 
  const char *opname; /* Temporary ??? */
  int oldpc = PC; /* PC at instruction start. */

  /* If the instruction is at the beginning of an exception handler,
     replace the top of the stack with the thrown object reference.  */
  if (instruction_bits [PC] & BCODE_EXCEPTION_TARGET)
    {
      /* Note that the verifier will not emit a type map at all for
	 dead exception handlers.  In this case we just ignore the
	 situation.  */
      if ((instruction_bits[PC] & BCODE_VERIFIED) != 0)
	{
	  tree type = pop_type (promote_type (throwable_type_node));
	  push_value (build_exception_object_ref (type));
	}
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
    (void) saw_index; /* Avoid set but not used warning.  */		\
    build_java_ret							\
      (find_local_variable (index, return_address_type_node, oldpc));	\
  }

#define JSR(OPERAND_TYPE, OPERAND_VALUE) \
  {						    \
    /* OPERAND_VALUE may have side-effects on PC */ \
    int opvalue = OPERAND_VALUE;		    \
    build_java_jsr (oldpc + opvalue, PC);	    \
  }

/* Push a constant onto the stack. */
#define PUSHC(OPERAND_TYPE, OPERAND_VALUE) \
  { int saw_index = 0;  int ival = (OPERAND_VALUE); \
    if (saw_index) java_push_constant_from_pool (current_jcf, ival); \
    else expand_java_pushc (ival, OPERAND_TYPE##_type_node); }

/* internal macro added for use by the WIDE case */
#define LOAD_INTERNAL(OPTYPE, OPVALUE) \
  expand_load_internal (OPVALUE, type_map[OPVALUE], oldpc);

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
    tree switch_expr = expand_java_switch (selector, oldpc + default_offset); \
    while (--npairs >= 0) \
      { \
	jint match = IMMEDIATE_s4; jint offset = IMMEDIATE_s4; \
	expand_java_add_case (switch_expr, match, oldpc + offset); \
      } \
  }

#define TABLE_SWITCH \
  { jint default_offset = IMMEDIATE_s4; \
    jint low = IMMEDIATE_s4; jint high = IMMEDIATE_s4; \
    tree selector = pop_value (INT_type_node); \
    tree switch_expr = expand_java_switch (selector, oldpc + default_offset); \
    for (; low <= high; low++) \
      { \
        jint offset = IMMEDIATE_s4; \
	expand_java_add_case (switch_expr, low, oldpc + offset); \
      } \
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
    tree klass = get_class_constant (current_jcf, IMMEDIATE_u2 );	\
    int  ndims = IMMEDIATE_u1;					\
    expand_java_multianewarray( klass, ndims );			\
  }

#define UNOP(OPERAND_TYPE, OPERAND_VALUE) \
  push_value (fold_build1 (NEGATE_EXPR, OPERAND_TYPE##_type_node, \
			   pop_value (OPERAND_TYPE##_type_node)));

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
#define STORE_INTERNAL(OPTYPE, OPVALUE)				\
  {								\
    tree decl, value;						\
    int index = OPVALUE;					\
    tree type = OPTYPE;						\
    value = pop_value (type);					\
    type = TREE_TYPE (value);					\
    decl = find_local_variable (index, type, oldpc);		\
    set_local_type (index, type);				\
    java_add_stmt (build2 (MODIFY_EXPR, type, decl, value));	\
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
    java_add_stmt (c);				\
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
        error ("unrecognized wide sub-instruction"); \
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

/* Return the opcode at PC in the code section pointed to by
   CODE_OFFSET.  */

static unsigned char
peek_opcode_at_pc (JCF *jcf, int code_offset, int pc)
{
  unsigned char opcode;
  long absolute_offset = (long)JCF_TELL (jcf);

  JCF_SEEK (jcf, code_offset);
  opcode = jcf->read_ptr [pc];
  JCF_SEEK (jcf, absolute_offset);
  return opcode;
}

/* Some bytecode compilers are emitting accurate LocalVariableTable
   attributes. Here's an example:
   
     PC   <t>store_<n>
     PC+1 ...
     
     Attribute "LocalVariableTable"
     slot #<n>: ... (PC: PC+1 length: L)
   
   This is accurate because the local in slot <n> really exists after
   the opcode at PC is executed, hence from PC+1 to PC+1+L.

   This procedure recognizes this situation and extends the live range
   of the local in SLOT to START_PC-1 or START_PC-2 (depending on the
   length of the store instruction.)

   This function is used by `give_name_to_locals' so that a local's
   DECL features a DECL_LOCAL_START_PC such that the first related
   store operation will use DECL as a destination, not an unrelated
   temporary created for the occasion.

   This function uses a global (instruction_bits) `note_instructions' should
   have allocated and filled properly.  */

int
maybe_adjust_start_pc (struct JCF *jcf, int code_offset,
		       int start_pc, int slot)
{
  int first, index, opcode;
  int pc, insn_pc;
  int wide_found = 0;

  if (!start_pc)
    return start_pc;

  first = index = -1;

  /* Find last previous instruction and remember it */
  for (pc = start_pc-1; pc; pc--) 
    if (instruction_bits [pc] & BCODE_INSTRUCTION_START)
      break;
  insn_pc = pc;

  /* Retrieve the instruction, handle `wide'. */  
  opcode = (int) peek_opcode_at_pc (jcf, code_offset, pc++);
  if (opcode == OPCODE_wide)
    {
      wide_found = 1;
      opcode = (int) peek_opcode_at_pc (jcf, code_offset, pc++);
    }

  switch (opcode)
    {
    case OPCODE_astore_0:
    case OPCODE_astore_1:
    case OPCODE_astore_2:
    case OPCODE_astore_3:
      first = OPCODE_astore_0;
      break;

    case OPCODE_istore_0:
    case OPCODE_istore_1:
    case OPCODE_istore_2:
    case OPCODE_istore_3:
      first = OPCODE_istore_0;
      break;
      
    case OPCODE_lstore_0:
    case OPCODE_lstore_1:
    case OPCODE_lstore_2:
    case OPCODE_lstore_3:
      first = OPCODE_lstore_0;
      break;

    case OPCODE_fstore_0:
    case OPCODE_fstore_1:
    case OPCODE_fstore_2:
    case OPCODE_fstore_3:
      first = OPCODE_fstore_0;
      break;

    case OPCODE_dstore_0:
    case OPCODE_dstore_1:
    case OPCODE_dstore_2:
    case OPCODE_dstore_3:
      first = OPCODE_dstore_0;
      break;

    case OPCODE_astore:
    case OPCODE_istore:
    case OPCODE_lstore:
    case OPCODE_fstore:
    case OPCODE_dstore:
      index = peek_opcode_at_pc (jcf, code_offset, pc);
      if (wide_found)
	{
	  int other = peek_opcode_at_pc (jcf, code_offset, ++pc);
	  index = (other << 8) + index;
	}
      break;
    }

  /* Now we decide: first >0 means we have a <t>store_<n>, index >0
     means we have a <t>store. */
  if ((first > 0 && opcode - first == slot) || (index > 0 && index == slot))
    start_pc = insn_pc;

  return start_pc;
}

/* Build a node to represent empty statements and blocks. */

tree
build_java_empty_stmt (void)
{
  tree t = build_empty_stmt (input_location);
  return t;
}

/* Promote all args of integral type before generating any code.  */

static void
promote_arguments (void)
{
  int i;
  tree arg;
  for (arg = DECL_ARGUMENTS (current_function_decl), i = 0;
       arg != NULL_TREE;  arg = DECL_CHAIN (arg), i++)
    {
      tree arg_type = TREE_TYPE (arg);
      if (INTEGRAL_TYPE_P (arg_type)
	  && TYPE_PRECISION (arg_type) < 32)
	{
	  tree copy = find_local_variable (i, integer_type_node, -1);
	  java_add_stmt (build2 (MODIFY_EXPR, integer_type_node,
				 copy,
				 fold_convert (integer_type_node, arg)));
	}
      if (TYPE_IS_WIDE (arg_type))
	i++;
    }
}

/* Create a local variable that points to the constant pool.  */

static void
cache_cpool_data_ref (void)
{
  if (optimize)
    {
      tree cpool;
      tree d = build_constant_data_ref (flag_indirect_classes);
      tree cpool_ptr = build_decl (input_location, VAR_DECL, NULL_TREE, 
				   build_pointer_type (TREE_TYPE (d)));
      java_add_local_var (cpool_ptr);
      TREE_CONSTANT (cpool_ptr) = 1;

      java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (cpool_ptr), 
			     cpool_ptr, build_address_of (d)));
      cpool = build1 (INDIRECT_REF, TREE_TYPE (d), cpool_ptr);
      TREE_THIS_NOTRAP (cpool) = 1;
      TYPE_CPOOL_DATA_REF (output_class) = cpool;
    }
}

#include "gt-java-expr.h"
