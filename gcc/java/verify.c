/* Handle verification of bytecoded methods for the GNU compiler for 
   the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This bytecode verifier is an implementation of the bytecode
verification process described in section 4.9 of "The Java(TM) Virtual
Machine Specification", Second Edition, by Tim Lindholm and Frank Yellin,
published by Addison-Wesley in 1999.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "java-tree.h"
#include "javaop.h"
#include "java-opcodes.h"
#include "jcf.h"
#include "java-except.h"
#include "toplev.h"

static void push_pending_label (tree);
static tree merge_types (tree, tree);
static const char *check_pending_block (tree);
static void type_stack_dup (int, int);
static char *pop_argument_types (tree);

extern int stack_pointer;

/* During verification, start of the current subroutine (jsr target). */
tree current_subr;

/* A list of pending blocks, chained using  LABEL_PENDING_CHAIN.
   A pending block is one that has LABEL_CHANGED set, which means
   it requires (re-) verification. */
tree pending_blocks;

/* Append TARGET_LABEL to the pending_block stack unless already in it. */

static void
push_pending_label (tree target_label) 
{
  if (! LABEL_CHANGED (target_label))
    {
      LABEL_PENDING_CHAIN (target_label) = pending_blocks;
      pending_blocks = target_label;
      LABEL_CHANGED (target_label) = 1;
    }
}

/* Note that TARGET_LABEL is a possible successor instruction.
   Merge the type state etc.
   Return NULL on success, or an error message on failure. */

static const char *
check_pending_block (tree target_label)
{
  int changed = merge_type_state (target_label);

  if (changed)
    {
      if (changed < 0)
	return "types could not be merged";
      push_pending_label (target_label);
    }

  if (current_subr == NULL_TREE)
    {
      if (LABEL_IN_SUBR (target_label))
	return "might transfer control into subroutine";
    }
  else
    {
      if (LABEL_IN_SUBR (target_label))
	{
	  if (LABEL_SUBR_START (target_label) != current_subr)
	    return "transfer out of subroutine";
	}
      else if (! LABEL_VERIFIED (target_label))
	{
	  LABEL_IN_SUBR (target_label) = 1;
	  LABEL_SUBR_START (target_label) = current_subr;
	}
      else
	return "transfer out of subroutine";
    }
  return NULL;
}

/* Count the number of nested jsr calls needed to reach LABEL. */

static int
subroutine_nesting (tree label)
{
  int nesting = 0;
  while (label != NULL_TREE && LABEL_IN_SUBR (label))
    {
      if (! LABEL_IS_SUBR_START (label))
	label = LABEL_SUBR_START (label);
      label = LABEL_SUBR_CONTEXT (label);
      nesting++;
    }
  return nesting;
}

/* Return the "merged" types of TYPE1 and TYPE2.
   If either is primitive, the other must match (after promotion to int).
   For reference types, return the common super-class.
   Return TYPE_UNKNOWN if the types cannot be merged. */   

static tree
merge_types (tree type1, tree type2)
{
  if (type1 == type2)
    return type1;
  if (type1 == TYPE_UNKNOWN || type2 == TYPE_UNKNOWN
      || type1 == TYPE_RETURN_ADDR || type2 == TYPE_RETURN_ADDR)
    return TYPE_UNKNOWN;
  if (TREE_CODE (type1) == POINTER_TYPE && TREE_CODE (type2) == POINTER_TYPE)
    {
      int depth1, depth2;
      tree tt1, tt2;
      /* ptr_type_node is only used for a null reference,
	 which is compatible with any reference type. */
      if (type1 == ptr_type_node || type2 == object_ptr_type_node)
	return type2;
      if (type2 == ptr_type_node || type1 == object_ptr_type_node)
	return type1;

      tt1 = TREE_TYPE (type1);
      tt2 = TREE_TYPE (type2);

      /* If tt{1,2} haven't been properly loaded, now is a good time
         to do it. */
      if (!TYPE_SIZE (tt1))
	{
	  load_class (tt1, 1);
	  safe_layout_class (tt1);
	}

      if (!TYPE_SIZE (tt2))
	{
	  load_class (tt2, 1);
	  safe_layout_class (tt2);
	}

      if (TYPE_ARRAY_P (tt1) || TYPE_ARRAY_P (tt2))
	{
	  if (TYPE_ARRAY_P (tt1) == TYPE_ARRAY_P (tt2))
	    {
	      tree el_type1 = TYPE_ARRAY_ELEMENT (tt1);
	      tree el_type2 = TYPE_ARRAY_ELEMENT (tt2);
	      tree el_type = NULL_TREE;
	      if (el_type1 == el_type2)
		el_type = el_type1;
	      else if (TREE_CODE (el_type1) == POINTER_TYPE
		       && TREE_CODE (el_type2) == POINTER_TYPE)
		el_type = merge_types (el_type1, el_type2);
	      if (el_type != NULL_TREE)
		{
		  HOST_WIDE_INT len1 = java_array_type_length (tt1);
		  HOST_WIDE_INT len2 = java_array_type_length (tt2);
		  if (len1 != len2)
		    len1 = -1;
		  else if (el_type1 == el_type2)
		    return type1;
		  return promote_type (build_java_array_type (el_type, len1));
		}
	    }
	  return object_ptr_type_node;
	}

      if (CLASS_INTERFACE (TYPE_NAME (tt1)))
	{
	  /* FIXME: should see if two interfaces have a common
	     superinterface.  */
	  if (CLASS_INTERFACE (TYPE_NAME (tt2)))
	    {
	      /* This is a kludge, but matches what Sun's verifier does.
		 It can be tricked, but is safe as long as type errors
		 (i.e. interface method calls) are caught at run-time. */
	      return object_ptr_type_node;
	    }
	  else
	    {
	      if (can_widen_reference_to (tt2, tt1))
		return type1;
	      else
		return object_ptr_type_node;
	    }
	}
      else if (CLASS_INTERFACE (TYPE_NAME (tt2)))
	{
	  if (can_widen_reference_to (tt1, tt2))
	    return type2;
	  else
	    return object_ptr_type_node;
	}

      type1 = tt1;
      type2 = tt2;

      depth1 = class_depth (type1);
      depth2 = class_depth (type2);
      for ( ; depth1 > depth2;  depth1--)
	type1 = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (type1), 0));
      for ( ; depth2 > depth1;  depth2--)
	type2 = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (type2), 0));
      while (type1 != type2)
	{
	  type1 = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (type1), 0));
	  type2 = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (type2), 0));
	}
      return promote_type (type1);
    }
  if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2)
      && TYPE_PRECISION (type1) <= 32 && TYPE_PRECISION (type2) <= 32)
    return int_type_node;
  return TYPE_UNKNOWN;
}

/* Merge the current type state with that at LABEL.
   Return -1 if the states are incompatible (i.e. on error),
   0 if there was no change, and 1 if there was a change. */

int
merge_type_state (tree label)
{
  int nlocals = DECL_MAX_LOCALS (current_function_decl);
  int cur_length = stack_pointer + nlocals;
  tree vec = LABEL_TYPE_STATE (label);
  tree return_map;
  if (vec == NULL_TREE)
    {
      vec = make_tree_vec (cur_length);
      LABEL_TYPE_STATE (label) = vec;

      while (--cur_length >= 0)
	TREE_VEC_ELT (vec, cur_length) = type_map[cur_length];
      return 1;
    }
  else
    {
      int i;
      int changed = 0;
      if (LABEL_IS_SUBR_START (label) && LABEL_VERIFIED (label)
	  && current_subr != label)
	return_map = LABEL_RETURN_TYPE_STATE (label);
      else
	return_map = NULL_TREE;
      if (TREE_VEC_LENGTH (vec) != cur_length)
	{
	  return -1;
	}
      for (i = 0; i < cur_length; i++)
	{
	  tree old_type = TREE_VEC_ELT (vec, i);
	  tree new_type = merge_types (old_type, type_map[i]);
	  if (TREE_VEC_ELT (vec, i) != new_type)
	    {
	      /* If there has been a change, note that since we must re-verify.
		 However, if the label is the start of a subroutine,
		 we don't care about local variables that are neither
		 set nor used in the subroutine. */
	      if (return_map == NULL_TREE || i >= nlocals
		  || TREE_VEC_ELT (return_map, i) != TYPE_UNUSED
		  || (TYPE_IS_WIDE (new_type)
		      && TREE_VEC_ELT (return_map, i+1) != TYPE_UNUSED))
		changed = 1;
	    }
	  TREE_VEC_ELT (vec, i) = new_type;
	  if (new_type == TYPE_UNKNOWN)
	    {
	      if (i >= nlocals)
		return -1;
	    }
	  else if (TYPE_IS_WIDE (new_type))
	    i++;
	}
      return changed;
    }
}

/* Handle dup-like operations. */

static void
type_stack_dup (int size, int offset)
{
  tree type[4];
  int index;
  for (index = 0;  index < size + offset; index++)
    {
      type[index] = stack_type_map[stack_pointer - 1];
      if (type[index] == void_type_node)
	{
	  index++;
	  type[index] = stack_type_map[stack_pointer - 2];
	  if (! TYPE_IS_WIDE (type[index]))
	    abort ();
	  if (index == size || index == size + offset)
	    /* Dup operation splits 64-bit number.  */
	    abort ();
	}
      pop_type (type[index]);
    }
  for (index = size;  --index >= 0; )
    {
      if (type[index] != void_type_node)
	push_type (type[index]);
    }

  for (index = size + offset;  --index >= 0; )
    {
      if (type[index] != void_type_node)
	push_type (type[index]);
    }
}

/* This keeps track of a start PC and corresponding initial index.  */
struct pc_index
{
  int start_pc;
  int index;
};

/* This causes the next iteration to ignore the next instruction
   and look for some other unhandled instruction. */
#define INVALIDATE_PC (prevpc = -1, oldpc = PC, PC = INVALID_PC)
#define INVALID_PC (-1)

#define VERIFICATION_ERROR(MESSAGE) \
  do { message = MESSAGE;  goto verify_error; } while (0)

#define VERIFICATION_ERROR_WITH_INDEX(MESSAGE) \
  do { message = MESSAGE;  goto error_with_index; } while (0)

/* Recursive helper function to pop argument types during verification.
   ARG_TYPES is the list of formal parameter types.
   Return NULL on success and a freshly malloc'd error message on failure. */

static char *
pop_argument_types (tree arg_types)
{
  if (arg_types == end_params_node)
    return NULL;
  if (TREE_CODE (arg_types) == TREE_LIST)
    {
      char *message = pop_argument_types (TREE_CHAIN (arg_types));
      if (message == NULL)
	pop_type_0 (TREE_VALUE (arg_types), &message);
      return message;
    }
  abort ();
}

#define POP_TYPE(TYPE, MESSAGE) \
  do { pmessage = NULL;  pop_type_0 (TYPE, &pmessage); \
       if (pmessage != NULL) goto pop_type_error; \
  } while (0)

#define POP_TYPE_CONV(TYPE, POPPED_TYPE, MESSAGE) \
  do { pmessage = NULL;  POPPED_TYPE = pop_type_0 (TYPE, &pmessage); \
       if (pmessage != NULL) goto pop_type_error; \
  } while (0)

#define PUSH_TYPE(TYPE) \
  do { if (! push_type_0 (TYPE)) { goto stack_overflow; }} while (0)

#define PUSH_PENDING(LABEL) \
     do { tree tmplab = LABEL; \
          if ((message = check_pending_block (tmplab)) != NULL) \
            { oldpc = LABEL_PC (tmplab); goto verify_error; }} while (0)

#ifdef __GNUC__
#define CHECK_PC_IN_RANGE(PC) __extension__ \
  ({if (PC < 0 || PC > length) goto bad_pc; (void)1;})
#else
#define CHECK_PC_IN_RANGE(PC) (PC < 0 || PC > length ? (abort (), 0) : 1)
#endif

#define BCODE byte_ops


/* Verify the bytecodes of the current method, with the instructions
   starting at BYTE_OPS and LENGTH in number, from the class file pointed to
   by JCF.
   Return 1 on success, 0 on failure.  */
int
verify_jvm_instructions (JCF* jcf, const unsigned char *byte_ops, long length)
{
  tree label;
  int wide = 0;
  int op_code;
  int PC;
  int oldpc = 0; /* PC of start of instruction. */
  int prevpc = 0;  /* If >= 0, PC of previous instruction. */
  const char *message = 0;
  char *pmessage;
  int i;
  int index;
  unsigned char *p;
  struct eh_range *prev_eh_ranges = NULL_EH_RANGE;
  struct eh_range *eh_ranges;
  tree return_type = TREE_TYPE (TREE_TYPE (current_function_decl));
  struct pc_index *starts;
  int eh_count;

  jint int_value = -1;

  pending_blocks = NULL_TREE;

  current_subr = NULL_TREE;

  /* Handle the exception table.  */
  method_init_exceptions ();
  JCF_SEEK (jcf, DECL_CODE_OFFSET (current_function_decl) + length);
  eh_count = JCF_readu2 (jcf);

  starts = xmalloc (eh_count * sizeof (struct pc_index));
  for (i = 0; i < eh_count; ++i)
    {
      starts[i].start_pc = GET_u2 (jcf->read_ptr + 8 * i);
      starts[i].index = i;
    }

  for (i = 0; i < eh_count; ++i)
    {
      int start_pc, end_pc, handler_pc, catch_type;

      p = jcf->read_ptr + 8 * starts[i].index;

      start_pc = GET_u2 (p);
      end_pc = GET_u2 (p+2);
      handler_pc = GET_u2 (p+4);
      catch_type = GET_u2 (p+6);

      if (start_pc < 0 || start_pc >= length
	  || end_pc < 0 || end_pc > length || start_pc >= end_pc
	  || handler_pc < 0 || handler_pc >= length
	  || ! (instruction_bits[start_pc] & BCODE_INSTRUCTION_START)
	  || (end_pc < length &&
	     ! (instruction_bits[end_pc] & BCODE_INSTRUCTION_START))
	  || ! (instruction_bits[handler_pc] & BCODE_INSTRUCTION_START))
	{
	  error ("bad pc in exception_table");
	  free (starts);
	  return 0;
	}

      add_handler (start_pc, end_pc,
		   lookup_label (handler_pc),
		   catch_type == 0 ? NULL_TREE
		   : get_class_constant (jcf, catch_type));

      instruction_bits[handler_pc] |= BCODE_EXCEPTION_TARGET;
    }

  free (starts);

  for (PC = 0;;)
    {
      tree type, tmp;

      if (((PC != INVALID_PC
	   && instruction_bits[PC] & BCODE_TARGET) != 0)
	  || PC == 0)
	{
	  PUSH_PENDING (lookup_label (PC));
	  INVALIDATE_PC;
	}

      /* Check if there are any more pending blocks in the current
	 subroutine.  Because we push pending blocks in a
	 last-in-first-out order, and because we don't push anything
	 from our caller until we are done with this subroutine or
	 anything nested in it, we are done if the top of the
	 pending_blocks stack is not in a subroutine, or it is in our
	 caller. */
      if (current_subr && PC == INVALID_PC)
	{
	  if (pending_blocks == NULL_TREE
	      || (subroutine_nesting (pending_blocks)
		  < subroutine_nesting (current_subr)))
	    {
	      int size
                = DECL_MAX_LOCALS (current_function_decl) + stack_pointer;

	      tree ret_map = LABEL_RETURN_TYPE_STATE (current_subr);
	      tmp = LABEL_RETURN_LABELS (current_subr);
	      
	      /* FIXME: If we exit a subroutine via a throw, we might
		 have returned to an earlier caller.  Obviously a
		 "ret" can only return one level, but a throw may
		 return many levels.  */
	      current_subr = LABEL_SUBR_CONTEXT (current_subr);

	      if (RETURN_MAP_ADJUSTED (ret_map))
		{
		  /* Since we are done with this subroutine, set up
		     the (so far known) return address as pending -
		     with the merged type state.  */
		  for ( ; tmp != NULL_TREE;  tmp = TREE_CHAIN (tmp))
		    {
		      tree return_label = TREE_VALUE (tmp);
		      tree return_state = LABEL_TYPE_STATE (return_label);
		      if (return_state == NULL_TREE)
			{
			  /* This means we had not verified the subroutine
                             earlier, so this is the first jsr to call it.
                             In this case, the type_map of the return
			     address is just the current type_map - and that
			     is handled by the following PUSH_PENDING.  */
			}
		      else
			{
			  /* In this case we have to do a merge.  But first
			     restore the type_map for unused slots to those
			     that were in effect at the jsr.  */
			  for (index = size; --index >= 0; )
			    {
			      type_map[index]
                                = TREE_VEC_ELT (ret_map, index);

			      if (type_map[index] == TYPE_UNUSED)
				type_map[index]
				  = TREE_VEC_ELT (return_state, index);
			    }
			}
		      PUSH_PENDING (return_label);
		    }
		}
	    }
	}

      if (PC == INVALID_PC)
	{
	  label = pending_blocks;

	  if (label == NULL_TREE)
	    break;  /* We're done! */

	  pending_blocks = LABEL_PENDING_CHAIN (label);
	  LABEL_CHANGED (label) = 0;

	  if (LABEL_IN_SUBR (label))
	    current_subr = LABEL_SUBR_START (label);
	  else
	    current_subr = NULL_TREE;

	  /* Restore type_map and stack_pointer from
	     LABEL_TYPE_STATE (label), and continue
	     compiling from there.  */
	  load_type_state (label);

	  PC = LABEL_PC (label);
	}
      else if (PC >= length)
	VERIFICATION_ERROR ("falling through the end of the method");


      oldpc = PC;

      if (! (instruction_bits[PC] & BCODE_INSTRUCTION_START) && ! wide)
	VERIFICATION_ERROR ("PC not at instruction start");

      instruction_bits[PC] |= BCODE_VERIFIED;

      eh_ranges = find_handler (oldpc);

      op_code = byte_ops[PC++];
      switch (op_code)
	{
	  int is_static, is_putting;

	case OPCODE_nop:
	  break;

	case OPCODE_iconst_m1:
	case OPCODE_iconst_0:	case OPCODE_iconst_1:	case OPCODE_iconst_2:
	case OPCODE_iconst_3:	case OPCODE_iconst_4:	case OPCODE_iconst_5:
	  i = op_code - OPCODE_iconst_0;
	  goto push_int;
	push_int:
	  if (byte_ops[PC] == OPCODE_newarray
	      || byte_ops[PC] == OPCODE_anewarray)
	    int_value = i;
	  PUSH_TYPE (int_type_node);  break;

	case OPCODE_lconst_0:	case OPCODE_lconst_1:
	  PUSH_TYPE (long_type_node);  break;

	case OPCODE_fconst_0:	case OPCODE_fconst_1:	case OPCODE_fconst_2:
	  PUSH_TYPE (float_type_node);  break;

	case OPCODE_dconst_0:	case OPCODE_dconst_1:
	  PUSH_TYPE (double_type_node);  break;

	case OPCODE_bipush:
	  i = IMMEDIATE_s1;
	  goto push_int;

	case OPCODE_sipush:
	  i = IMMEDIATE_s2;
	  goto push_int;

	case OPCODE_iload:  type = int_type_node;  goto general_load;
	case OPCODE_lload:  type = long_type_node;  goto general_load;
	case OPCODE_fload:  type = float_type_node;  goto general_load;
	case OPCODE_dload:  type = double_type_node;  goto general_load;
	case OPCODE_aload:  type = ptr_type_node;  goto general_load;
	general_load:
	index = wide ? IMMEDIATE_u2 : IMMEDIATE_u1;
	wide = 0;
	goto load;
	case OPCODE_iload_0:  type = int_type_node;  index = 0; goto load;
	case OPCODE_iload_1:  type = int_type_node;  index = 1; goto load;
	case OPCODE_iload_2:  type = int_type_node;  index = 2; goto load;
	case OPCODE_iload_3:  type = int_type_node;  index = 3; goto load;
	case OPCODE_lload_0:  type = long_type_node; index = 0; goto load;
	case OPCODE_lload_1:  type = long_type_node; index = 1; goto load;
	case OPCODE_lload_2:  type = long_type_node; index = 2; goto load;
	case OPCODE_lload_3:  type = long_type_node; index = 3; goto load;
	case OPCODE_fload_0:  type = float_type_node; index = 0; goto load;
	case OPCODE_fload_1:  type = float_type_node; index = 1; goto load;
	case OPCODE_fload_2:  type = float_type_node; index = 2; goto load;
	case OPCODE_fload_3:  type = float_type_node; index = 3; goto load;
	case OPCODE_dload_0: type = double_type_node; index = 0; goto load;
	case OPCODE_dload_1: type = double_type_node; index = 1; goto load;
	case OPCODE_dload_2: type = double_type_node; index = 2; goto load;
	case OPCODE_dload_3: type = double_type_node; index = 3; goto load;
	case OPCODE_aload_0:  type = ptr_type_node;  index = 0;  goto load;
	case OPCODE_aload_1:  type = ptr_type_node;  index = 1;  goto load;
	case OPCODE_aload_2:  type = ptr_type_node;  index = 2;  goto load;
	case OPCODE_aload_3:  type = ptr_type_node;  index = 3;  goto load;
	load:
	if (index < 0
	    || (index + TYPE_IS_WIDE (type)
		>= DECL_MAX_LOCALS (current_function_decl)))
	  VERIFICATION_ERROR_WITH_INDEX
	    ("invalid local variable index %d in load");
	tmp = type_map[index];
	if (tmp == TYPE_UNKNOWN)
	  VERIFICATION_ERROR_WITH_INDEX
	    ("loading local variable %d which has unknown type");
	else if (tmp == TYPE_SECOND
	    || (TYPE_IS_WIDE (type)
		&& type_map[index+1] != void_type_node)
	    || (type == ptr_type_node
		? TREE_CODE (tmp) != POINTER_TYPE
		: type == int_type_node
		? (! INTEGRAL_TYPE_P (tmp) || TYPE_PRECISION (tmp) > 32)
		: type != tmp))
	  VERIFICATION_ERROR_WITH_INDEX
	    ("loading local variable %d which has invalid type");
	PUSH_TYPE (tmp);
	goto note_used;
	case OPCODE_istore:  type = int_type_node;  goto general_store;
	case OPCODE_lstore:  type = long_type_node;  goto general_store;
	case OPCODE_fstore:  type = float_type_node;  goto general_store;
	case OPCODE_dstore:  type = double_type_node;  goto general_store;
	case OPCODE_astore:  type = object_ptr_type_node;  goto general_store;
	general_store:
	index = wide ? IMMEDIATE_u2 : IMMEDIATE_u1;
	wide = 0;
	goto store;
	case OPCODE_istore_0:  type = int_type_node; index = 0; goto store;
	case OPCODE_istore_1:  type = int_type_node; index = 1; goto store;
	case OPCODE_istore_2:  type = int_type_node; index = 2; goto store;
	case OPCODE_istore_3:  type = int_type_node; index = 3; goto store;
	case OPCODE_lstore_0:  type = long_type_node; index=0; goto store;
	case OPCODE_lstore_1:  type = long_type_node; index=1; goto store;
	case OPCODE_lstore_2:  type = long_type_node; index=2; goto store;
	case OPCODE_lstore_3:  type = long_type_node; index=3; goto store;
	case OPCODE_fstore_0:  type=float_type_node; index=0; goto store;
	case OPCODE_fstore_1:  type=float_type_node; index=1; goto store;
	case OPCODE_fstore_2:  type=float_type_node; index=2; goto store;
	case OPCODE_fstore_3:  type=float_type_node; index=3; goto store;
	case OPCODE_dstore_0:  type=double_type_node; index=0; goto store;
	case OPCODE_dstore_1:  type=double_type_node; index=1; goto store;
	case OPCODE_dstore_2:  type=double_type_node; index=2; goto store;
	case OPCODE_dstore_3:  type=double_type_node; index=3; goto store;
	case OPCODE_astore_0:  type = ptr_type_node; index = 0; goto store;
	case OPCODE_astore_1:  type = ptr_type_node; index = 1; goto store;
	case OPCODE_astore_2:  type = ptr_type_node; index = 2; goto store;
	case OPCODE_astore_3:  type = ptr_type_node; index = 3; goto store;
	store:
	if (index < 0
	    || (index + TYPE_IS_WIDE (type)
		>= DECL_MAX_LOCALS (current_function_decl)))
	  {
	    VERIFICATION_ERROR_WITH_INDEX
	      ("invalid local variable index %d in store");
	    return 0;
	  }
	POP_TYPE_CONV (type, type, NULL);
	type_map[index] = type;

	/* If a local variable has changed, we need to reconsider exception
        handlers.  */
	prev_eh_ranges = NULL_EH_RANGE;

	/* Allocate decl for this variable now, so we get a temporary
! 	   that survives the whole method. */
	find_local_variable (index, type, oldpc);

        if (TYPE_IS_WIDE (type))
          type_map[index+1] = TYPE_SECOND;

	/* ... fall through to note_used ... */
	note_used:
	  /* For store or load, note that local variable INDEX is used.
	     This is needed to verify try-finally subroutines. */
	  if (current_subr)
	    {
	      tree vec = LABEL_RETURN_TYPE_STATE (current_subr);
	      tree subr_vec = LABEL_TYPE_STATE (current_subr);
	      int len = 1 + TYPE_IS_WIDE (type);
	      while (--len >= 0)
		{
		  if (TREE_VEC_ELT (vec, index) == TYPE_UNUSED)
		    TREE_VEC_ELT (vec, index) = TREE_VEC_ELT (subr_vec, index);
		}
	    }
	break;
	case OPCODE_iadd:
	case OPCODE_iand:
	case OPCODE_idiv:
	case OPCODE_imul:
	case OPCODE_ior:
	case OPCODE_irem:
	case OPCODE_ishl:
	case OPCODE_ishr:
	case OPCODE_isub:
	case OPCODE_iushr:
	case OPCODE_ixor:
	  type = int_type_node;  goto binop;
	case OPCODE_ineg:
	case OPCODE_i2c:
	case OPCODE_i2b:
	case OPCODE_i2s:
	  type = int_type_node;  goto unop;
	case OPCODE_ladd:
	case OPCODE_land:
	case OPCODE_ldiv:
	case OPCODE_lsub:
	case OPCODE_lmul:
	case OPCODE_lrem:
	case OPCODE_lor:
	case OPCODE_lxor:
	  type = long_type_node;  goto binop;
	case OPCODE_lneg:
	  type = long_type_node;  goto unop;
	case OPCODE_fadd:	case OPCODE_fsub:
	case OPCODE_fmul:	case OPCODE_fdiv:	case OPCODE_frem:
	  type = float_type_node;  goto binop;
	case OPCODE_fneg:
	  type = float_type_node;  goto unop;
	case OPCODE_dadd:	case OPCODE_dsub:
	case OPCODE_dmul:	case OPCODE_ddiv:	case OPCODE_drem:
	  type = double_type_node;  goto binop;
	case OPCODE_dneg:
	  type = double_type_node;  goto unop;

	unop:
	  pop_type (type);
	  PUSH_TYPE (type);
	  break;

	binop:
	  pop_type (type);
	  pop_type (type);
	  PUSH_TYPE (type);
	  break;

	case OPCODE_lshl:
	case OPCODE_lshr:
	case OPCODE_lushr:
	  pop_type (int_type_node);
	  pop_type (long_type_node);
	  PUSH_TYPE (long_type_node);
	  break;

	case OPCODE_iinc:
	  index = wide ? IMMEDIATE_u2 : IMMEDIATE_u1;
	  PC += wide + 1;
	  wide = 0;
	  if (index < 0 || index >= DECL_MAX_LOCALS (current_function_decl))
	    VERIFICATION_ERROR ("invalid local variable index in iinc");
	  tmp = type_map[index];
	  if (tmp == NULL_TREE
	      || ! INTEGRAL_TYPE_P (tmp) || TYPE_PRECISION (tmp) > 32)
	    VERIFICATION_ERROR ("invalid local variable type in iinc");
	  break;

	case OPCODE_i2l:
	  pop_type (int_type_node);    PUSH_TYPE (long_type_node);   break;
	case OPCODE_i2f:
	  pop_type (int_type_node);    PUSH_TYPE (float_type_node);  break;
	case OPCODE_i2d:
	  pop_type (int_type_node);    PUSH_TYPE (double_type_node); break;
	case OPCODE_l2i:
	  pop_type (long_type_node);   PUSH_TYPE (int_type_node);    break;
	case OPCODE_l2f:
	  pop_type (long_type_node);   PUSH_TYPE (float_type_node);  break;
	case OPCODE_l2d:
	  pop_type (long_type_node);   PUSH_TYPE (double_type_node); break;
	case OPCODE_f2i:
	  pop_type (float_type_node);  PUSH_TYPE (int_type_node);    break;
	case OPCODE_f2l:
	  pop_type (float_type_node);  PUSH_TYPE (long_type_node);   break;
	case OPCODE_f2d:
	  pop_type (float_type_node);  PUSH_TYPE (double_type_node); break;
	case OPCODE_d2i:
	  pop_type (double_type_node); PUSH_TYPE (int_type_node);    break;
	case OPCODE_d2l:
	  pop_type (double_type_node); PUSH_TYPE (long_type_node);   break;
	case OPCODE_d2f:
	  pop_type (double_type_node); PUSH_TYPE (float_type_node);  break;

	case OPCODE_lcmp:
	  type = long_type_node;  goto compare;
	case OPCODE_fcmpl:
	case OPCODE_fcmpg:
	  type = float_type_node;  goto compare;
	case OPCODE_dcmpl:
	case OPCODE_dcmpg:
	  type = double_type_node;  goto compare;
	compare:
	  pop_type (type);  pop_type (type);
	  PUSH_TYPE (int_type_node);  break;

	case OPCODE_ifeq:
	case OPCODE_ifne:
	case OPCODE_iflt:
	case OPCODE_ifge:
	case OPCODE_ifgt:
	case OPCODE_ifle:
	  pop_type (int_type_node);  goto cond;
	case OPCODE_ifnull:
	case OPCODE_ifnonnull:
	  pop_type (ptr_type_node ); goto cond;
	case OPCODE_if_icmpeq:
	case OPCODE_if_icmpne:
	case OPCODE_if_icmplt:
	case OPCODE_if_icmpge:
	case OPCODE_if_icmpgt:
	case OPCODE_if_icmple:
	  pop_type (int_type_node);  pop_type (int_type_node);  goto cond;
	case OPCODE_if_acmpeq:
	case OPCODE_if_acmpne:
	  pop_type (object_ptr_type_node);  pop_type (object_ptr_type_node);
	  goto cond;

	cond:
	  PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s2));
	  break;
          
	case OPCODE_goto:
	  PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s2));
	  INVALIDATE_PC;
	  break;

	case OPCODE_wide:
	  switch (byte_ops[PC])
	    {
	    case OPCODE_iload:  case OPCODE_lload:
	    case OPCODE_fload:  case OPCODE_dload:  case OPCODE_aload:
	    case OPCODE_istore:  case OPCODE_lstore:
	    case OPCODE_fstore:  case OPCODE_dstore:  case OPCODE_astore:
	    case OPCODE_iinc:
	    case OPCODE_ret:
	      wide = 1;
	      break;
	    default:
	      VERIFICATION_ERROR ("invalid use of wide instruction");
	    }
	  break;

	case OPCODE_return:   type = void_type_node;   goto ret;
	case OPCODE_ireturn:
	  if ((TREE_CODE (return_type) == BOOLEAN_TYPE
	       || TREE_CODE (return_type) == CHAR_TYPE
	       || TREE_CODE (return_type) == INTEGER_TYPE)
	      && TYPE_PRECISION (return_type) <= 32)
	    type = return_type;
	  else
	    type = NULL_TREE;
	  goto ret;
	case OPCODE_lreturn:  type = long_type_node;   goto ret;
	case OPCODE_freturn:  type = float_type_node;  goto ret;
	case OPCODE_dreturn:  type = double_type_node; goto ret;
	case OPCODE_areturn:
	  if (TREE_CODE (return_type) == POINTER_TYPE)
	    type = return_type;
	  else
	    type = NULL_TREE;
	  goto ret;

	ret:
	  if (type != return_type)
	    VERIFICATION_ERROR ("incorrect ?return opcode");
	  if (type != void_type_node)
	    POP_TYPE (type, "return value has wrong type");
	  INVALIDATE_PC;
	  break;

	case OPCODE_getstatic: is_putting = 0;  is_static = 1;  goto field;
	case OPCODE_putstatic: is_putting = 1;  is_static = 1;  goto field;
	case OPCODE_getfield:  is_putting = 0;  is_static = 0;  goto field;
	case OPCODE_putfield:  is_putting = 1;  is_static = 0;  goto field;
	field:
	  {
	    tree field_signature, field_type;
	    index = IMMEDIATE_u2;

	    if (index <= 0 || index >= JPOOL_SIZE (current_jcf))
	      VERIFICATION_ERROR_WITH_INDEX ("bad constant pool index %d");

	    if (JPOOL_TAG (current_jcf, index) != CONSTANT_Fieldref)
	      VERIFICATION_ERROR
		("field instruction does not reference a Fieldref");

	    field_signature
              = COMPONENT_REF_SIGNATURE (&current_jcf->cpool, index);

	    field_type = get_type_from_signature (field_signature);

	    if (is_putting)
	      POP_TYPE (field_type, "incorrect type for field");

	    if (! is_static)
	      {
		int clindex
                  = COMPONENT_REF_CLASS_INDEX (&current_jcf->cpool, index);

		tree self_type = get_class_constant (current_jcf, clindex);

		/* Defer actual checking until next pass. */
		POP_TYPE (self_type, "incorrect type for field reference");
	      }

	    if (! is_putting)
	      PUSH_TYPE (field_type);
	    break;
	  }

	case OPCODE_new:
	  PUSH_TYPE (get_class_constant (jcf, IMMEDIATE_u2));
	  break;

	case OPCODE_dup:     wide = 1; index = 0;  goto dup;
	case OPCODE_dup_x1:  wide = 1; index = 1;  goto dup;
	case OPCODE_dup_x2:  wide = 1; index = 2;  goto dup;
	case OPCODE_dup2:    wide = 2; index = 0;  goto dup;
	case OPCODE_dup2_x1: wide = 2; index = 1;  goto dup;
	case OPCODE_dup2_x2: wide = 2; index = 2;  goto dup;

	dup:
	  if (wide + index > stack_pointer)
	    VERIFICATION_ERROR ("stack underflow - dup* operation");
	  type_stack_dup (wide, index);
	  wide = 0;
	  break;

	case OPCODE_pop:  index = 1;  goto pop;
	case OPCODE_pop2: index = 2;  goto pop;

	pop:
	  if (stack_pointer < index)
	    VERIFICATION_ERROR ("stack underflow");
	  stack_pointer -= index;
	  break;

	case OPCODE_swap:
	  if (stack_pointer < 2)
	    VERIFICATION_ERROR ("stack underflow (in swap)");
	  else
	    {
	      tree type1 = stack_type_map[stack_pointer - 1];
	      tree type2 = stack_type_map[stack_pointer - 2];

	      if (type1 == void_type_node || type2 == void_type_node)
		VERIFICATION_ERROR ("verifier (swap):  double or long value");

	      stack_type_map[stack_pointer - 2] = type1;
	      stack_type_map[stack_pointer - 1] = type2;
	    }
	  break;

	case OPCODE_ldc:   index = IMMEDIATE_u1;  goto ldc;
	case OPCODE_ldc2_w:
	case OPCODE_ldc_w:
	  index = IMMEDIATE_u2;  goto ldc;

	ldc:
	  if (index <= 0 || index >= JPOOL_SIZE (current_jcf))
	    VERIFICATION_ERROR_WITH_INDEX ("bad constant pool index %d in ldc");

	  int_value = -1;
	  switch (JPOOL_TAG (current_jcf, index) & ~CONSTANT_ResolvedFlag)
	    {
	    case CONSTANT_Integer:  type = int_type_node;  goto check_ldc;
	    case CONSTANT_Float:    type = float_type_node;  goto check_ldc;
	    case CONSTANT_String:   type = string_type_node; goto check_ldc;
	    case CONSTANT_Long:    type = long_type_node;    goto check_ldc;
	    case CONSTANT_Double:  type = double_type_node;  goto check_ldc;
	    check_ldc:
	      if (TYPE_IS_WIDE (type) == (op_code == OPCODE_ldc2_w))
		break;
	      /* ... else fall through ... */
	    default:
	      VERIFICATION_ERROR ("bad constant pool tag in ldc");
	    }
	  if (type == int_type_node)
	    {
	      i = TREE_INT_CST_LOW (get_constant (current_jcf, index));
	      goto push_int;
	    }
	  PUSH_TYPE (type);
	  break;

	case OPCODE_invokevirtual:
	case OPCODE_invokespecial:
	case OPCODE_invokestatic:
	case OPCODE_invokeinterface:
	  {
	    tree sig, method_name, method_type, self_type;
	    int self_is_interface, tag;
	    index = IMMEDIATE_u2;

	    if (index <= 0 || index >= JPOOL_SIZE (current_jcf))
	      VERIFICATION_ERROR_WITH_INDEX
		("bad constant pool index %d for invoke");

	    tag = JPOOL_TAG (current_jcf, index);

	    if (op_code == OPCODE_invokeinterface)
	      {
		if (tag != CONSTANT_InterfaceMethodref)
		  VERIFICATION_ERROR
		    ("invokeinterface does not reference an InterfaceMethodref");
	      }
	    else
	      {
		if (tag != CONSTANT_Methodref)
		  VERIFICATION_ERROR ("invoke does not reference a Methodref");
	      }

	    sig = COMPONENT_REF_SIGNATURE (&current_jcf->cpool, index);

	    self_type
              = get_class_constant (current_jcf,
                                    COMPONENT_REF_CLASS_INDEX
                                      (&current_jcf->cpool, index));

	    if (! CLASS_LOADED_P (self_type))
	      load_class (self_type, 1);

	    self_is_interface = CLASS_INTERFACE (TYPE_NAME (self_type));
	    method_name = COMPONENT_REF_NAME (&current_jcf->cpool, index);
	    method_type = parse_signature_string ((const unsigned char *) IDENTIFIER_POINTER (sig),
						  IDENTIFIER_LENGTH (sig));

	    if (TREE_CODE (method_type) != FUNCTION_TYPE)
	      VERIFICATION_ERROR ("bad method signature");

	    pmessage = pop_argument_types (TYPE_ARG_TYPES (method_type));
	    if (pmessage != NULL)
	      {
		message = "invalid argument type";
		goto pop_type_error;
	      }

	    /* Can't invoke <clinit>.  */
	    if (ID_CLINIT_P (method_name))
	      VERIFICATION_ERROR ("invoke opcode can't invoke <clinit>");

	    /* Apart from invokespecial, can't invoke <init>.  */
	    if (op_code != OPCODE_invokespecial && ID_INIT_P (method_name))
	      VERIFICATION_ERROR ("invoke opcode can't invoke <init>");

	    if (op_code != OPCODE_invokestatic)
	      POP_TYPE (self_type,
			"stack type not subclass of invoked method's class");

	    switch (op_code)
	      {
	      case OPCODE_invokeinterface:
	        {
		  int nargs    = IMMEDIATE_u1;
		  int notZero  = IMMEDIATE_u1;
		
		  if (!nargs || notZero)
		      VERIFICATION_ERROR 
		        ("invalid argument number in invokeinterface");

		  /* If we verify/resolve the constant pool, as we should,
		     this test (and the one just following) are redundant.  */
		  if (! self_is_interface)
		    VERIFICATION_ERROR
                      ("invokeinterface calls method not in interface");
		  break;

		default:
		  if (self_is_interface)
		    VERIFICATION_ERROR ("method in interface called");
		}
	      }

	    if (TREE_TYPE (method_type) != void_type_node)
	      PUSH_TYPE (TREE_TYPE (method_type));
	    break;
	  }

	case OPCODE_arraylength:
	    /* Type checking actually made during code generation.  */
	    pop_type (ptr_type_node);
	    PUSH_TYPE (int_type_node);
	    break;
	    
        /* Q&D verification *or* more checking done during code generation
	   for byte/boolean/char/short, the value popped is a int coerced
	   into the right type before being stored.  */
	case OPCODE_iastore: type = int_type_node;     goto astore;
	case OPCODE_lastore: type = long_type_node;    goto astore;
	case OPCODE_fastore: type = float_type_node;   goto astore;
	case OPCODE_dastore: type = double_type_node;  goto astore;
	case OPCODE_aastore: type = ptr_type_node;     goto astore;
	case OPCODE_bastore: type = int_type_node; goto astore;
	case OPCODE_castore: type = int_type_node; goto astore;
	case OPCODE_sastore: type = int_type_node; goto astore;

	astore:
	  /* FIXME - need better verification here.  */
	  pop_type (type);	     /* new value */
	  pop_type (int_type_node);  /* index */
	  pop_type (ptr_type_node);  /* array */
	  break;

        /* Q&D verification *or* more checking done during code generation
	   for byte/boolean/char/short, the value pushed is a int.  */
	case OPCODE_iaload: type = int_type_node;     goto aload;
	case OPCODE_laload: type = long_type_node;    goto aload;
	case OPCODE_faload: type = float_type_node;   goto aload;
	case OPCODE_daload: type = double_type_node;  goto aload;
	case OPCODE_aaload: type = ptr_type_node;     goto aload;
	case OPCODE_baload: type = promote_type (byte_type_node);  goto aload;
	case OPCODE_caload: type = promote_type (char_type_node);  goto aload;
	case OPCODE_saload: type = promote_type (short_type_node); goto aload;

        aload:
	  pop_type (int_type_node);
	  tmp = pop_type (ptr_type_node);
	  if (is_array_type_p (tmp))
	    type = TYPE_ARRAY_ELEMENT (TREE_TYPE (tmp));
	  else if (tmp != TYPE_NULL)
	    VERIFICATION_ERROR ("array load from non-array type");
	  PUSH_TYPE (type);
	  break;

	case OPCODE_anewarray:
	  type = get_class_constant (current_jcf, IMMEDIATE_u2);
	  type = promote_type (type);
	  goto newarray;

	case OPCODE_newarray:
	  index = IMMEDIATE_u1;
	  type = decode_newarray_type (index);
	  if (type == NULL_TREE)
	    VERIFICATION_ERROR ("invalid type code in newarray opcode");
	  goto newarray;

	newarray:
	  if (int_value >= 0 && prevpc >= 0)
	    {
	      /* If the previous instruction pushed an int constant,
		 we want to use it. */
	      switch (byte_ops[prevpc])
		{
		case OPCODE_iconst_0: case OPCODE_iconst_1:
		case OPCODE_iconst_2: case OPCODE_iconst_3:
		case OPCODE_iconst_4: case OPCODE_iconst_5:
		case OPCODE_bipush:  case OPCODE_sipush:
		case OPCODE_ldc: case OPCODE_ldc_w:
		  break;
		default:
		  int_value = -1;
		}
	    }
	  else
	    int_value = -1;

	  type = build_java_array_type (type, int_value);
	  pop_type (int_type_node);
	  PUSH_TYPE (type);
	  break;

	case OPCODE_multianewarray:
	  {
	    int ndim, i;
	    index = IMMEDIATE_u2;
	    ndim  = IMMEDIATE_u1;

            if (ndim < 1)
              VERIFICATION_ERROR
                ("number of dimension lower that 1 in multianewarray" );

	    for (i = 0; i < ndim; i++)
	      pop_type (int_type_node);

	    PUSH_TYPE (get_class_constant (current_jcf, index));
	    break;
	  }

	case OPCODE_aconst_null:
	  PUSH_TYPE (ptr_type_node);
	  break;

	case OPCODE_athrow:
	  /* FIXME: athrow also empties the stack.  */
	  POP_TYPE (throwable_type_node, "missing throwable at athrow" );
	  INVALIDATE_PC;
	  break;

	case OPCODE_checkcast:
	  POP_TYPE (object_ptr_type_node,
		    "checkcast operand is not a pointer");
	  type = get_class_constant (current_jcf, IMMEDIATE_u2);
	  PUSH_TYPE (type);
	  break;

	case OPCODE_instanceof:
	  POP_TYPE (object_ptr_type_node,
		    "instanceof operand is not a pointer");
	  get_class_constant (current_jcf, IMMEDIATE_u2);
	  PUSH_TYPE (int_type_node);
	  break;

	case OPCODE_tableswitch:
	  {
	    jint low, high;

	    POP_TYPE (int_type_node, "missing int for tableswitch");

	    while (PC%4)
	      {
	        if (byte_ops[PC++])
		  VERIFICATION_ERROR ("bad alignment in tableswitch pad");
	      }

	    PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s4));
	    low  = IMMEDIATE_s4;
	    high = IMMEDIATE_s4;

	    if (low > high)
	      VERIFICATION_ERROR ("unsorted low/high value in tableswitch");

	    while (low++ <= high)
	      PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s4));

	    INVALIDATE_PC;
	    break;
	  }

	case OPCODE_lookupswitch:
	  {
	    jint npairs, last = 0, not_registered = 1;

	    POP_TYPE (int_type_node, "missing int for lookupswitch");

	    while (PC%4)
	      {
	        if (byte_ops[PC++])
		  VERIFICATION_ERROR ("bad alignment in lookupswitch pad");
	      }

	    PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s4));
	    npairs = IMMEDIATE_s4;
	    
	    if (npairs < 0)
	      VERIFICATION_ERROR ("invalid number of targets in lookupswitch");

	    while (npairs--)
	      {
	        int match = IMMEDIATE_s4;

		if (not_registered)
		  not_registered = 0;
		else if (last >= match)
		  VERIFICATION_ERROR ("unsorted match value in lookupswitch");

		last = match;
		PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s4));
	      }
	    INVALIDATE_PC;
	    break;
	  }

	case OPCODE_monitorenter: 
	  /* fall thru */
	case OPCODE_monitorexit:
	  pop_type (ptr_type_node);
	  break;

	case OPCODE_goto_w:
	  PUSH_PENDING (lookup_label (oldpc + IMMEDIATE_s4));
	  INVALIDATE_PC;
	  break;

	case OPCODE_jsr:
	  {
	    tree target = lookup_label (oldpc + IMMEDIATE_s2);
	    tree return_label = lookup_label (PC);
	    PUSH_TYPE (return_address_type_node);
	    /* The return label chain will be null if this is the first
	       time we've seen this jsr target.  */
            if (LABEL_RETURN_LABEL (target) == NULL_TREE)
	      {
		tree return_type_map;
		int nlocals = DECL_MAX_LOCALS (current_function_decl);
		index = nlocals + DECL_MAX_STACK (current_function_decl);
		return_type_map = make_tree_vec (index);

		while (index > nlocals)
		  TREE_VEC_ELT (return_type_map, --index) = TYPE_UNKNOWN;

		while (index > 0)
		  TREE_VEC_ELT (return_type_map, --index) = TYPE_UNUSED;

		LABEL_RETURN_LABEL (target)
		  = build_decl (LABEL_DECL, NULL_TREE, TREE_TYPE (target));
		LABEL_PC (LABEL_RETURN_LABEL (target)) = INVALID_PC;
		LABEL_RETURN_TYPE_STATE (target) = return_type_map;
		LABEL_IS_SUBR_START (target) = 1;
		LABEL_IN_SUBR (target) = 1;
		LABEL_SUBR_START (target) = target;
		LABEL_SUBR_CONTEXT (target) = current_subr;
	      }
	    else if (! LABEL_IS_SUBR_START (target)
		     || LABEL_SUBR_CONTEXT (target) != current_subr)
	      VERIFICATION_ERROR ("label part of different subroutines");

	    i = merge_type_state (target);
	    if (i != 0)
	      {
		if (i < 0)
		  VERIFICATION_ERROR ("types could not be merged at jsr");
		push_pending_label (target);
	      }
	    current_subr = target;

	    /* Chain return_pc onto LABEL_RETURN_LABELS (target) if needed. */
	    if (! value_member (return_label, LABEL_RETURN_LABELS (target)))
	      {
		LABEL_RETURN_LABELS (target)
		  = tree_cons (NULL_TREE, return_label,
			       LABEL_RETURN_LABELS (target));
	      }

	    if (LABEL_VERIFIED (target))
	      {
		tree return_map = LABEL_RETURN_TYPE_STATE (target);
		int len = TREE_VEC_LENGTH (return_map);
		stack_pointer = len - DECL_MAX_LOCALS (current_function_decl);
		while (--len >= 0)
		  {
		    if (TREE_VEC_ELT (return_map, len) != TYPE_UNUSED)
		      type_map[len] = TREE_VEC_ELT (return_map, len);
		  }
		current_subr = LABEL_SUBR_CONTEXT (target);
		if (RETURN_MAP_ADJUSTED (return_map))
		  PUSH_PENDING (return_label);
	      }

	    INVALIDATE_PC;
	  }
	  break;

	case OPCODE_ret:
	  if (current_subr == NULL_TREE)
	    VERIFICATION_ERROR ("ret instruction not in a jsr subroutine");
	  else
	    {
	      tree ret_map = LABEL_RETURN_TYPE_STATE (current_subr);
	      int size
                = DECL_MAX_LOCALS (current_function_decl) + stack_pointer;
	      index = wide ? IMMEDIATE_u2 : IMMEDIATE_u1;
	      wide = 0;
	      INVALIDATE_PC;
	      if (index < 0 || index >= DECL_MAX_LOCALS (current_function_decl)
		  || type_map[index] != TYPE_RETURN_ADDR)
		VERIFICATION_ERROR ("invalid ret index");

	      /* The next chunk of code is similar to an inlined version of
               merge_type_state (LABEL_RETURN_LABEL (current_subr)).
	       The main differences are that LABEL_RETURN_LABEL is
	       pre-allocated by the jsr (but we don't know the size then);
	       and that we have to handle TYPE_UNUSED.  */

	      if (! RETURN_MAP_ADJUSTED (ret_map))
		{
                  /* First return from this subroutine - fix stack
                  pointer.  */
		  TREE_VEC_LENGTH (ret_map) = size;
		  for (index = size;  --index >= 0; )
		    {
		      if (TREE_VEC_ELT (ret_map, index) != TYPE_UNUSED)
			TREE_VEC_ELT (ret_map, index) = type_map[index];
		    }
		  RETURN_MAP_ADJUSTED (ret_map) = 1;
		}
	      else
		{
		  if (TREE_VEC_LENGTH (ret_map) != size)
		    VERIFICATION_ERROR ("inconsistent stack size on ret");
		  for (index = 0;  index < size;  index++)
		    {
		      tree type = TREE_VEC_ELT (ret_map, index);
		      if (type != TYPE_UNUSED)
			{
			  type = merge_types (type, type_map[index]);
			  TREE_VEC_ELT (ret_map, index) = type;
			  if (type == TYPE_UNKNOWN)
			    {
			      if (index >= size - stack_pointer)
				VERIFICATION_ERROR
				  ("inconsistent types on ret from jsr");
			    }
			  else if (TYPE_IS_WIDE (type))
			    index++;
			}
		    }
		}
            }
          break;

        case OPCODE_jsr_w:        
        case OPCODE_ret_w:
        default:
          error ("unknown opcode %d@pc=%d during verification", op_code, PC-1);
          return 0;
        }

      prevpc = oldpc;

      /* The following test is true if we have entered or exited an exception
	 handler range *or* we have done a store to a local variable.
	 In either case we need to consider any exception handlers that
	 might "follow" this instruction.  */

      if (eh_ranges != prev_eh_ranges)
	{
	  int save_stack_pointer = stack_pointer;
	  int index = DECL_MAX_LOCALS (current_function_decl);
	  tree save_type = type_map[index];
	  tree save_current_subr = current_subr;
	  struct eh_range *ranges = find_handler (oldpc);
	  stack_pointer = 1;

	  for ( ; ranges != NULL_EH_RANGE; ranges = ranges->outer)
	    {
	      tree chain = ranges->handlers;

	      /* We need to determine if the handler is part of current_subr.
		 The are two cases:  (1) The exception catch range
		 is entirely within current_subr.  In that case the handler
		 is also part of current_subr.
		 (2) Some of the catch range is not in current_subr.
		 In that case, the handler is *not* part of current_subr.

		 Figuring out which is the case is not necessarily obvious,
		 in the presence of clever code generators (and obfuscators).
		 We make a simplifying assumption that in case (2) we
		 have that the current_subr is entirely within the catch range.
		 In that case we can assume if that if a caller (the jsr) of
		 a subroutine is within the catch range, then the handler is
		 *not* part of the subroutine, and vice versa.  */

	      current_subr = save_current_subr;
	      for ( ; current_subr != NULL_TREE;
		    current_subr = LABEL_SUBR_CONTEXT (current_subr))
		{
		  tree return_labels = LABEL_RETURN_LABELS (current_subr);
		  /* There could be multiple return_labels, but
		     we only need to check one.  */
		  int return_pc = LABEL_PC (TREE_VALUE (return_labels));
		  if (return_pc <= ranges->start_pc
		      || return_pc > ranges->end_pc)
		    break;
		}

	      for ( ; chain != NULL_TREE; chain = TREE_CHAIN (chain))
		{
		  tree handler = TREE_VALUE (chain);
		  tree type = TREE_PURPOSE (chain);

		  if (type == NULL_TREE)  /* a finally handler */
		    type = throwable_type_node;

		  type_map[index] = promote_type (type);

		  PUSH_PENDING (handler);
		}
	    }
	  stack_pointer = save_stack_pointer;
	  current_subr = save_current_subr;
	  type_map[index] = save_type;
	  prev_eh_ranges = eh_ranges;
	}
    }

  return 1;

 pop_type_error:
  error ("verification error at PC=%d", oldpc);
  if (message != NULL)
    error ("%s", message);
  error ("%s", pmessage);
  free (pmessage);
  return 0;

 stack_overflow:
  message = "stack overflow";
  goto verify_error;

 bad_pc:
  message = "program counter out of range";
  goto verify_error;

 error_with_index:
  error ("verification error at PC=%d", oldpc);
  error (message, index);
  return 0;

 verify_error:
  error ("verification error at PC=%d", oldpc);
  error ("%s", message);
  return 0;
}
