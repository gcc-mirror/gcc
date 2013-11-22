/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains the CilkPlus Intrinsics
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Balaji V. Iyer <balaji.v.iyer@intel.com>,
   Intel Corporation

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "expr.h"
#include "optabs.h"
#include "recog.h"
#include "tree-iterator.h"
#include "gimplify.h"
#include "cilk.h"

/* This structure holds all the important fields of the internal structures,
   internal built-in functions, and Cilk-specific data types.  Explanation of 
   all the these fielsd are given in cilk.h.  */
tree cilk_trees[(int) CILK_TI_MAX];

/* Returns the value in structure FRAME pointed by the FIELD_NUMBER
   (e.g. X.y).  
   FIELD_NUMBER is an index to the structure FRAME_PTR.  For details
   about these fields, refer to cilk_trees structure in cilk.h and
   cilk_init_builtins function  in this file.  Returns a TREE that is the type 
   of the field represented by FIELD_NUMBER.  If VOLATIL parameter is set
   to true then the returning field is set as volatile.  */

tree
cilk_dot (tree frame, int field_number, bool volatil)
{
  tree field = cilk_trees[field_number];
  field = fold_build3 (COMPONENT_REF, TREE_TYPE (field), frame, field, 
		       NULL_TREE);
  TREE_THIS_VOLATILE (field) = volatil;
  return field;
}

/* Returns the address of a field in FRAME_PTR, pointed by FIELD_NUMBER.  
   (e.g. (&X)->y).   Please see cilk_dot function for explanation of the 
   FIELD_NUMBER.  Returns a tree that is the type of the field represented 
   by FIELD_NUMBER. If VOLATIL parameter is set to true then the returning
   field is set as volatile.  */

tree
cilk_arrow (tree frame_ptr, int field_number, bool volatil)
{
  return cilk_dot (fold_build1 (INDIRECT_REF, 
				TREE_TYPE (TREE_TYPE (frame_ptr)), frame_ptr), 
		   field_number, volatil);
}


/* This function will add FIELD of type TYPE to a defined built-in 
   structure.  *NAME is the name of the field to be added.  */

static tree
add_field (const char *name, tree type, tree fields)
{
  tree t = get_identifier (name);
  tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL, t, type);
  TREE_CHAIN (field) = fields;
  return field;
}

/* This function will define a built-in function of NAME, of type FNTYPE and
   register it under the built-in function code CODE.  If PUBLISH is set then
   the declaration is pushed into the declaration list.  CODE is the index
   to the cilk_trees array.  *NAME is the name of the function to be added.  */

static tree
install_builtin (const char *name, tree fntype, enum built_in_function code,
                 bool publish)
{
  tree fndecl = build_fn_decl (name, fntype);
  DECL_BUILT_IN_CLASS (fndecl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (fndecl) = code;
  if (publish)
    {
      tree t = lang_hooks.decls.pushdecl (fndecl);
      if (t)
        fndecl = t;
    }
  set_builtin_decl (code, fndecl, true);
  return fndecl;
}

/* Creates and initializes all the built-in Cilk keywords functions and three
   structures: __cilkrts_stack_frame, __cilkrts_pedigree and __cilkrts_worker.
   Detailed information about __cilkrts_stack_frame and
   __cilkrts_worker structures are given in libcilkrts/include/internal/abi.h.
   __cilkrts_pedigree is described in libcilkrts/include/cilk/common.h.  */

void
cilk_init_builtins (void)
{
  /* Now build the following __cilkrts_pedigree struct:
     struct __cilkrts_pedigree {
        uint64_t rank;
        struct __cilkrts_pedigree *parent;
      }  */
       
  tree pedigree_type = lang_hooks.types.make_type (RECORD_TYPE);
  tree pedigree_ptr  = build_pointer_type (pedigree_type);
  tree field = add_field ("rank", uint64_type_node, NULL_TREE);
  cilk_trees[CILK_TI_PEDIGREE_RANK] = field;
  field = add_field ("parent", pedigree_ptr, field);
  cilk_trees[CILK_TI_PEDIGREE_PARENT] = field;
  finish_builtin_struct (pedigree_type, "__cilkrts_pedigree_GCC", field,
			 NULL_TREE);
  lang_hooks.types.register_builtin_type (pedigree_type,
					  "__cilkrts_pedigree_t");
  cilk_pedigree_type_decl = pedigree_type; 
  
  /* Build the Cilk Stack Frame:
     struct __cilkrts_stack_frame {
       uint32_t flags;
       uint32_t size;
       struct __cilkrts_stack_frame *call_parent;
       __cilkrts_worker *worker;
       void *except_data;
       void *ctx[4];
       uint32_t mxcsr;
       uint16_t fpcsr;
       uint16_t reserved;
       __cilkrts_pedigree pedigree;
     };  */

  tree frame = lang_hooks.types.make_type (RECORD_TYPE);
  tree frame_ptr = build_pointer_type (frame);
  tree worker_type = lang_hooks.types.make_type (RECORD_TYPE);
  tree worker_ptr = build_pointer_type (worker_type);
  tree s_type_node = build_int_cst (size_type_node, 4);

  tree flags = add_field ("flags", uint32_type_node, NULL_TREE);
  tree size = add_field ("size", uint32_type_node, flags);
  tree parent = add_field ("call_parent", frame_ptr, size);
  tree worker = add_field ("worker", worker_ptr, parent);
  tree except = add_field ("except_data", frame_ptr, worker);
  tree context = add_field ("ctx",
			    build_array_type (ptr_type_node,
					      build_index_type (s_type_node)),
			    except);
  tree mxcsr = add_field ("mxcsr", uint32_type_node, context);
  tree fpcsr = add_field ("fpcsr", uint16_type_node, mxcsr);
  tree reserved = add_field ("reserved", uint16_type_node, fpcsr);
  tree pedigree = add_field ("pedigree", pedigree_type, reserved);
  
  /* Now add them to a common structure whose fields are #defined to something
     that is used at a later stage.  */
  cilk_trees[CILK_TI_FRAME_FLAGS] = flags;
  cilk_trees[CILK_TI_FRAME_PARENT] = parent;
  cilk_trees[CILK_TI_FRAME_WORKER] = worker;
  cilk_trees[CILK_TI_FRAME_EXCEPTION] = except;
  cilk_trees[CILK_TI_FRAME_CONTEXT] = context;
  /* We don't care about reserved, so no need to store it in cilk_trees.  */
  cilk_trees[CILK_TI_FRAME_PEDIGREE] = pedigree;
  TREE_ADDRESSABLE (frame) = 1;

  finish_builtin_struct (frame, "__cilkrts_st_frame_GCC", pedigree, NULL_TREE);
  cilk_frame_type_decl = frame;
  lang_hooks.types.register_builtin_type (frame, "__cilkrts_frame_t");

  cilk_frame_ptr_type_decl = build_qualified_type (frame_ptr,
						   TYPE_QUAL_VOLATILE);
  /* Now let's do the following worker struct:

     struct __cilkrts_worker {
       __cilkrts_stack_frame *volatile *volatile tail;
       __cilkrts_stack_frame *volatile *volatile head;
       __cilkrts_stack_frame *volatile *volatile exc;
       __cilkrts_stack_frame *volatile *volatile protected_tail;
       __cilkrts_stack_frame *volatile *ltq_limit;
       int32_t self;
       global_state_t *g;
       local_state *l;
       cilkred_map *reducer_map;
       __cilkrts_stack_frame *current_stack_frame;
       void *reserved;
       __cilkrts_worker_sysdep_state *sysdep;
       __cilkrts_pedigree pedigree;
    }   */

  tree fptr_volatil_type = build_qualified_type (frame_ptr, TYPE_QUAL_VOLATILE);
  tree fptr_volatile_ptr = build_pointer_type (fptr_volatil_type);
  tree fptr_vol_ptr_vol = build_qualified_type (fptr_volatile_ptr,
						TYPE_QUAL_VOLATILE);
  tree g = lang_hooks.types.make_type (RECORD_TYPE);
  finish_builtin_struct (g, "__cilkrts_global_state", NULL_TREE, NULL_TREE);
  tree l = lang_hooks.types.make_type (RECORD_TYPE);
  finish_builtin_struct (l, "__cilkrts_local_state", NULL_TREE, NULL_TREE);
  tree sysdep_t = lang_hooks.types.make_type (RECORD_TYPE);
  finish_builtin_struct (sysdep_t, "__cilkrts_worker_sysdep_state", NULL_TREE,
			 NULL_TREE);
  
  field = add_field ("tail", fptr_vol_ptr_vol, NULL_TREE);
  cilk_trees[CILK_TI_WORKER_TAIL] = field;
  field = add_field ("head", fptr_vol_ptr_vol, field);
  field  = add_field ("exc", fptr_vol_ptr_vol, field);
  field = add_field ("protected_tail", fptr_vol_ptr_vol, field);
  field = add_field ("ltq_limit", fptr_volatile_ptr, field);
  field = add_field ("self", integer_type_node, field);
  field = add_field ("g", build_pointer_type (g), field);
  field = add_field ("l", build_pointer_type (g), field);
  field = add_field ("reducer_map", ptr_type_node, field);
  field = add_field ("current_stack_frame", frame_ptr, field);
  cilk_trees[CILK_TI_WORKER_CUR] = field;
  field = add_field ("saved_protected_tail", fptr_volatile_ptr, field);
  field = add_field ("sysdep", build_pointer_type (sysdep_t), field);
  field = add_field ("pedigree", pedigree_type, field);
  cilk_trees[CILK_TI_WORKER_PEDIGREE] = field;
  finish_builtin_struct (worker_type, "__cilkrts_worker_GCC", field,
			 NULL_TREE);

  tree fptr_arglist = tree_cons (NULL_TREE, frame_ptr, void_list_node);
  tree fptr_fun = build_function_type (void_type_node, fptr_arglist);
  
  /* void __cilkrts_enter_frame_1 (__cilkrts_stack_frame *);  */
  cilk_enter_fndecl = install_builtin ("__cilkrts_enter_frame_1", fptr_fun,
				       BUILT_IN_CILK_ENTER_FRAME, false);

  /* void __cilkrts_enter_frame_fast_1 (__cilkrts_stack_frame *);  */
  cilk_enter_fast_fndecl = 
    install_builtin ("__cilkrts_enter_frame_fast_1", fptr_fun, 
		     BUILT_IN_CILK_ENTER_FRAME_FAST, false);
  
  /* void __cilkrts_pop_frame (__cilkrts_stack_frame *);  */
  cilk_pop_fndecl = install_builtin ("__cilkrts_pop_frame", fptr_fun,
				     BUILT_IN_CILK_POP_FRAME, false);

  /* void __cilkrts_leave_frame (__cilkrts_stack_frame *);  */
  cilk_leave_fndecl = install_builtin ("__cilkrts_leave_frame", fptr_fun,
				       BUILT_IN_CILK_LEAVE_FRAME, false);

  /* void __cilkrts_sync (__cilkrts_stack_frame *);  */
  cilk_sync_fndecl = install_builtin ("__cilkrts_sync", fptr_fun,
				      BUILT_IN_CILK_SYNC, false);

  /* void __cilkrts_detach (__cilkrts_stack_frame *);  */
  cilk_detach_fndecl = install_builtin ("__cilkrts_detach", fptr_fun,
					BUILT_IN_CILK_DETACH, false);

  /* __cilkrts_rethrow (struct stack_frame *);  */
  cilk_rethrow_fndecl = install_builtin ("__cilkrts_rethrow", fptr_fun, 
					 BUILT_IN_CILK_RETHROW, false);

  /* __cilkrts_save_fp_ctrl_state (__cilkrts_stack_frame *);  */
  cilk_save_fp_fndecl = install_builtin ("__cilkrts_save_fp_ctrl_state", 
					 fptr_fun, BUILT_IN_CILK_SAVE_FP,
					 false);
}

/* Get the appropriate frame arguments for CALL that is of type CALL_EXPR.  */

static tree
get_frame_arg (tree call)
{
  tree arg, argtype;

  gcc_assert (call_expr_nargs (call) >= 1);
    
  arg = CALL_EXPR_ARG (call, 0);
  argtype = TREE_TYPE (arg);
  gcc_assert (TREE_CODE (argtype) == POINTER_TYPE);

  argtype = TREE_TYPE (argtype);
  
  gcc_assert (!lang_hooks.types_compatible_p
	      || lang_hooks.types_compatible_p (argtype, cilk_frame_type_decl));

  /* If it is passed in as an address, then just use the value directly 
     since the function is inlined.  */
  if (TREE_CODE (arg) == INDIRECT_REF || TREE_CODE (arg) == ADDR_EXPR)
    return TREE_OPERAND (arg, 0);
  return arg;
}

/* Expands the __cilkrts_pop_frame function call stored in EXP.  */

void
expand_builtin_cilk_pop_frame (tree exp)
{
  tree frame = get_frame_arg (exp);
  tree parent = cilk_dot (frame, CILK_TI_FRAME_PARENT, 0);

  tree clear_parent = build2 (MODIFY_EXPR, void_type_node, parent,
			      build_int_cst (TREE_TYPE (parent), 0));
  expand_expr (clear_parent, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* During LTO, the is_cilk_function flag gets cleared.
     If __cilkrts_pop_frame is called, then this definitely must be a
     cilk function.  */
  if (cfun)
    cfun->is_cilk_function = 1;
}

/* Expands the cilk_detach function call stored in EXP.  */

void
expand_builtin_cilk_detach (tree exp)
{
  rtx insn;
  tree fptr = get_frame_arg (exp);

  if (fptr == NULL_TREE)
    return;

  tree parent = cilk_dot (fptr, CILK_TI_FRAME_PARENT, 0);
  tree worker = cilk_dot (fptr, CILK_TI_FRAME_WORKER, 0);
  tree tail = cilk_dot (worker, CILK_TI_WORKER_TAIL, 1);

  rtx wreg = expand_expr (worker, NULL_RTX, Pmode, EXPAND_NORMAL);
  if (GET_CODE (wreg) != REG)
    wreg = copy_to_reg (wreg);
  rtx preg = expand_expr (parent, NULL_RTX, Pmode, EXPAND_NORMAL);

  /* TMP <- WORKER.TAIL
    *TMP <- PARENT
     TMP <- TMP + 1
     WORKER.TAIL <- TMP   */

  HOST_WIDE_INT worker_tail_offset =
    tree_to_shwi (DECL_FIELD_OFFSET (cilk_trees[CILK_TI_WORKER_TAIL])) +
    tree_to_shwi (DECL_FIELD_BIT_OFFSET (cilk_trees[CILK_TI_WORKER_TAIL])) /
    BITS_PER_UNIT;
  rtx tmem0 = gen_rtx_MEM (Pmode,
			   plus_constant (Pmode, wreg, worker_tail_offset));
  set_mem_attributes (tmem0, tail, 0);
  MEM_NOTRAP_P (tmem0) = 1;
  gcc_assert (MEM_VOLATILE_P (tmem0));
  rtx treg = copy_to_mode_reg (Pmode, tmem0);
  rtx tmem1 = gen_rtx_MEM (Pmode, treg);
  set_mem_attributes (tmem1, TREE_TYPE (TREE_TYPE (tail)), 0);
  MEM_NOTRAP_P (tmem1) = 1;
  emit_move_insn (tmem1, preg);
  emit_move_insn (treg, plus_constant (Pmode, treg, GET_MODE_SIZE (Pmode)));

  /* There is a release barrier (st8.rel, membar #StoreStore,
     sfence, lwsync, etc.) between the two stores.  On x86
     normal volatile stores have proper semantics; the sfence
     would only be needed for nontemporal stores (which we
     could generate using the storent optab, for no benefit
     in this case).

     The predicate may return false even for a REG if this is
     the limited release operation that only stores 0.  */
  enum insn_code icode = direct_optab_handler (sync_lock_release_optab, Pmode); 
  if (icode != CODE_FOR_nothing
      && insn_data[icode].operand[1].predicate (treg, Pmode)
      && (insn = GEN_FCN (icode) (tmem0, treg)) != NULL_RTX)
    emit_insn (insn);
  else
    emit_move_insn (tmem0, treg);

  /* The memory barrier inserted above should not prevent
     the load of flags from being moved before the stores,
     but in practice it does because it is implemented with
     unspec_volatile.  In-order RISC machines should
     explicitly load flags earlier.  */

  tree flags = cilk_dot (fptr, CILK_TI_FRAME_FLAGS, 0);
  expand_expr (build2 (MODIFY_EXPR, void_type_node, flags,
		       build2 (BIT_IOR_EXPR, TREE_TYPE (flags), flags,
			       build_int_cst (TREE_TYPE (flags),
					      CILK_FRAME_DETACHED))),
	       const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Returns a setjmp CALL_EXPR with FRAME->context as its parameter.  */

tree
cilk_call_setjmp (tree frame)
{
  tree c = cilk_dot (frame, CILK_TI_FRAME_CONTEXT, false);
  c = build1 (ADDR_EXPR, build_pointer_type (ptr_type_node), c);
  return build_call_expr (builtin_decl_implicit (BUILT_IN_SETJMP), 1, c);
}

/* This function will expand the _Cilk_sync keyword.  */

static tree
expand_cilk_sync (void)
{
  tree frame = cfun->cilk_frame_decl;

  /* Cilk_sync is converted to the following code:

     sf.pedigree = sf.worker->pedigree;
     if (frame.flags & CILK_FRAME_UNSYNCHED)
     {
        __cilkrts_save_fp_state (&sf);
        if (!builtin_setjmp (sf.ctx) 
	    __cilkrts_sync (&sf); 
	else 
	   if (sf.flags & CILK_FRAME_EXCEPTING) 
	     __cilkrts_rethrow (&sf); 
      }
      sf.worker->pedigree.rank = sf.worker->pedigree.rank + 1;  */

  tree flags = cilk_dot (frame, CILK_TI_FRAME_FLAGS, false);
  
  tree unsynched = fold_build2 (BIT_AND_EXPR, TREE_TYPE (flags), flags,
				build_int_cst (TREE_TYPE (flags),
					       CILK_FRAME_UNSYNCHED));

  unsynched = fold_build2 (NE_EXPR, TREE_TYPE (unsynched), unsynched,
			   build_int_cst (TREE_TYPE (unsynched), 0));

  tree frame_addr = build1 (ADDR_EXPR, cilk_frame_ptr_type_decl, frame);

  /* Check if exception (0x10) bit is set in the sf->flags.  */
  tree except_flag = fold_build2 (BIT_AND_EXPR, TREE_TYPE (flags), flags,
				  build_int_cst (TREE_TYPE (flags),
						 CILK_FRAME_EXCEPTING));
  except_flag = fold_build2 (NE_EXPR, TREE_TYPE (except_flag), except_flag,
			     build_int_cst (TREE_TYPE (except_flag), 0));

  /* If the exception flag is set then call the __cilkrts_rethrow (&sf).  */
  tree except_cond = fold_build3 (COND_EXPR, void_type_node, except_flag,
				  build_call_expr (cilk_rethrow_fndecl, 1,
						   frame_addr),
				  build_empty_stmt (EXPR_LOCATION (unsynched)));
  
  tree sync_expr = build_call_expr (cilk_sync_fndecl, 1, frame_addr);
  tree setjmp_expr = cilk_call_setjmp (frame);
  setjmp_expr = fold_build2 (EQ_EXPR, TREE_TYPE (setjmp_expr), setjmp_expr,
			     build_int_cst (TREE_TYPE (setjmp_expr), 0));
  
  setjmp_expr = fold_build3 (COND_EXPR, void_type_node, setjmp_expr,
			     sync_expr, except_cond);
  tree sync_list = alloc_stmt_list ();
  append_to_statement_list (build_call_expr (cilk_save_fp_fndecl, 1,
					     frame_addr), &sync_list);
  append_to_statement_list (setjmp_expr, &sync_list);
  tree sync = fold_build3 (COND_EXPR, void_type_node, unsynched, sync_list,
			   build_empty_stmt (EXPR_LOCATION (unsynched)));
  tree parent_pedigree = cilk_dot (frame, CILK_TI_FRAME_PEDIGREE, false);
  tree worker = cilk_dot (frame, CILK_TI_FRAME_WORKER, false);
  tree worker_pedigree = cilk_arrow (worker, CILK_TI_WORKER_PEDIGREE, false);
  tree assign_pedigree = fold_build2 (MODIFY_EXPR, void_type_node,
				      parent_pedigree, worker_pedigree);
  tree w_ped_rank = cilk_dot (unshare_expr (worker_pedigree), 
			      CILK_TI_PEDIGREE_RANK, false);
  tree incr_ped_rank = fold_build2 (PLUS_EXPR, TREE_TYPE (w_ped_rank),
				    w_ped_rank,
				    build_one_cst (TREE_TYPE (w_ped_rank)));
  incr_ped_rank = fold_build2 (MODIFY_EXPR, void_type_node, w_ped_rank,
			       incr_ped_rank);
  tree ret_sync_exp = alloc_stmt_list ();
  append_to_statement_list (assign_pedigree, &ret_sync_exp);
  append_to_statement_list (sync, &ret_sync_exp);
  append_to_statement_list (incr_ped_rank, &ret_sync_exp);
  return ret_sync_exp;
}

/* Gimplifies the cilk_sync expression passed in *EXPR_P.  Returns GS_ALL_DONE 
   when finished.  */

void
gimplify_cilk_sync (tree *expr_p, gimple_seq *pre_p)
{
  tree sync_expr = expand_cilk_sync ();
  *expr_p = NULL_TREE;
  gimplify_and_add (sync_expr, pre_p);
}
