/* Internal to rs6000 type, variable, and function declarations and
   definitons shared between the various rs6000 source files.
   Copyright (C) 1991-2019 Free Software Foundation, Inc.
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
extern section *toc_section;

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
extern rtx create_TOC_reference (rtx symbol, rtx largetoc_reg);
extern rtx rs6000_got_sym (void);
extern struct machine_function *rs6000_init_machine_status (void);
extern bool save_reg_p (int reg);
extern const char * rs6000_machine_from_flags (void);
extern void emit_asm_machine (void);
extern bool rs6000_global_entry_point_prologue_needed_p (void);

/* Return true if the OFFSET is valid for the quad address instructions that
   use d-form (register + offset) addressing.  */

static inline bool
quad_address_offset_p (HOST_WIDE_INT offset)
{
  return (IN_RANGE (offset, -32768, 32767) && ((offset) & 0xf) == 0);
}


#endif
