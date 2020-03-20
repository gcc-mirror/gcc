/* Internal to rs6000 type, variable, and function declarations and
   definitons shared between the various rs6000 source files.
   Copyright (C) 1991-2020 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_RS6000_INTERNAL_H
#define GCC_RS6000_INTERNAL_H

/* Structure used to define the rs6000 stack */
typedef struct rs6000_stack {
  int reload_completed;		/* stack info won't change from here on */
  int first_gp_reg_save;	/* first callee saved GP register used */
  int first_fp_reg_save;	/* first callee saved FP register used */
  int first_altivec_reg_save;	/* first callee saved AltiVec register used */
  int lr_save_p;		/* true if the link reg needs to be saved */
  int cr_save_p;		/* true if the CR reg needs to be saved */
  unsigned int vrsave_mask;	/* mask of vec registers to save */
  int push_p;			/* true if we need to allocate stack space */
  int calls_p;			/* true if the function makes any calls */
  int world_save_p;		/* true if we're saving *everything*:
				   r13-r31, cr, f14-f31, vrsave, v20-v31  */
  enum rs6000_abi abi;		/* which ABI to use */
  int gp_save_offset;		/* offset to save GP regs from initial SP */
  int fp_save_offset;		/* offset to save FP regs from initial SP */
  int altivec_save_offset;	/* offset to save AltiVec regs from initial SP */
  int lr_save_offset;		/* offset to save LR from initial SP */
  int cr_save_offset;		/* offset to save CR from initial SP */
  int vrsave_save_offset;	/* offset to save VRSAVE from initial SP */
  int varargs_save_offset;	/* offset to save the varargs registers */
  int ehrd_offset;		/* offset to EH return data */
  int ehcr_offset;		/* offset to EH CR field data */
  int reg_size;			/* register size (4 or 8) */
  HOST_WIDE_INT vars_size;	/* variable save area size */
  int parm_size;		/* outgoing parameter size */
  int save_size;		/* save area size */
  int fixed_size;		/* fixed size of stack frame */
  int gp_size;			/* size of saved GP registers */
  int fp_size;			/* size of saved FP registers */
  int altivec_size;		/* size of saved AltiVec registers */
  int cr_size;			/* size to hold CR if not in fixed area */
  int vrsave_size;		/* size to hold VRSAVE */
  int altivec_padding_size;	/* size of altivec alignment padding */
  HOST_WIDE_INT total_size;	/* total bytes allocated for stack */
  int savres_strategy;
} rs6000_stack_t;


extern int need_toc_init;
extern char toc_label_name[10];
extern int rs6000_pic_labelno;

#ifdef USING_ELFOS_H
extern const char *rs6000_machine;
#endif


/* The VRSAVE bitmask puts bit %v0 as the most significant bit.  */
#define ALTIVEC_REG_BIT(REGNO) (0x80000000 >> ((REGNO) - FIRST_ALTIVEC_REGNO))


/* Declare functions in rs6000-logue.c or called in rs6000.c
   from rs6000-logue.c  */

extern int uses_TOC (void);
extern bool rs6000_global_entry_point_needed_p (void);
extern void rs6000_output_function_prologue (FILE *file);
extern void rs6000_output_function_epilogue (FILE *file);
extern bool rs6000_function_ok_for_sibcall (tree decl, tree exp);
extern sbitmap rs6000_get_separate_components (void);
extern sbitmap rs6000_components_for_bb (basic_block bb);
extern void rs6000_disqualify_components (sbitmap components, edge e,
					  sbitmap edge_components,
					  bool /*is_prologue*/);
extern void rs6000_emit_prologue_components (sbitmap components);
extern void rs6000_emit_epilogue_components (sbitmap components);
extern void rs6000_set_handled_components (sbitmap components);
extern rs6000_stack_t * rs6000_stack_info (void);
extern rtx rs6000_got_sym (void);
extern struct machine_function *rs6000_init_machine_status (void);
extern bool save_reg_p (int reg);
extern const char * rs6000_machine_from_flags (void);
extern void emit_asm_machine (void);
extern bool rs6000_global_entry_point_prologue_needed_p (void);
extern bool rs6000_keep_leaf_when_profiled (void);
extern void rs6000_live_on_entry (bitmap regs);

/* Return true if the OFFSET is valid for the quad address instructions that
   use d-form (register + offset) addressing.  */

static inline bool
quad_address_offset_p (HOST_WIDE_INT offset)
{
  return (IN_RANGE (offset, -32768, 32767) && ((offset) & 0xf) == 0);
}

/* Mach-O (Darwin) support for longcalls, emitted from  rs6000-logue.c.  */

#if TARGET_MACHO

typedef struct branch_island_d {
  tree function_name;
  tree label_name;
  int line_number;
 } branch_island;

extern vec<branch_island, va_gc> *branch_islands;

#endif

/* Declare functions in rs6000-call.c or called in rs6000.c
   from rs6000-call.c  */
extern int rs6000_darwin64_struct_check_p (machine_mode mode, const_tree type);
extern bool rs6000_discover_homogeneous_aggregate (machine_mode mode,
						   const_tree type,
						   machine_mode *elt_mode,
						   int *n_elts);
extern void rs6000_output_mi_thunk (FILE *file,
				    tree thunk_fndecl ATTRIBUTE_UNUSED,
				    HOST_WIDE_INT delta,
				    HOST_WIDE_INT vcall_offset,
				    tree function);
extern bool rs6000_output_addr_const_extra (FILE *file, rtx x);
extern bool rs6000_gimple_fold_builtin (gimple_stmt_iterator *gsi);
extern tree rs6000_build_builtin_va_list (void);
extern void rs6000_va_start (tree valist, rtx nextarg);
extern tree rs6000_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
				    gimple_seq *post_p);
extern machine_mode rs6000_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
						  machine_mode mode,
						  int *punsignedp ATTRIBUTE_UNUSED,
						  const_tree, int);
extern bool rs6000_return_in_memory (const_tree type, 
				     const_tree fntype ATTRIBUTE_UNUSED);
extern bool rs6000_return_in_msb (const_tree valtype);
extern bool rs6000_pass_by_reference (cumulative_args_t,
				      const function_arg_info &);
extern void setup_incoming_varargs (cumulative_args_t,
				    const function_arg_info &, int *, int);
extern unsigned int rs6000_function_arg_boundary (machine_mode mode,
						  const_tree type);
extern bool rs6000_must_pass_in_stack (const function_arg_info &);
extern int rs6000_arg_partial_bytes (cumulative_args_t,
				     const function_arg_info &);
extern void rs6000_function_arg_advance (cumulative_args_t,
					 const function_arg_info &);
extern pad_direction rs6000_function_arg_padding (machine_mode mode,
						  const_tree type);
extern rtx rs6000_function_arg (cumulative_args_t, const function_arg_info &);
extern rtx rs6000_darwin64_record_arg (CUMULATIVE_ARGS *, const_tree,
				       bool, bool);
extern rtx rs6000_internal_arg_pointer (void);

extern void rs6000_init_builtins (void);
extern tree rs6000_builtin_decl (unsigned code,
				 bool initialize_p ATTRIBUTE_UNUSED);
extern rtx rs6000_expand_builtin (tree exp, rtx target,
				  rtx subtarget ATTRIBUTE_UNUSED,
				  machine_mode mode ATTRIBUTE_UNUSED,
				  int ignore ATTRIBUTE_UNUSED);
extern tree rs6000_fold_builtin (tree fndecl ATTRIBUTE_UNUSED,
			         int n_args ATTRIBUTE_UNUSED,
			         tree *args ATTRIBUTE_UNUSED,
			         bool ignore ATTRIBUTE_UNUSED);

#if TARGET_ELF
extern bool rs6000_passes_ieee128;
#endif
extern bool rs6000_passes_float;
extern bool rs6000_passes_long_double;
extern bool rs6000_passes_vector;
extern bool rs6000_returns_struct;
extern bool cpu_builtin_p;

#endif
