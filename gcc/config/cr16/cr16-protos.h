/* Prototypes for exported functions defined in cr16.c
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by KPIT Cummins Infosystems Limited.

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

#ifndef GCC_CR16_PROTOS_H
#define GCC_CR16_PROTOS_H

/* Register usage.  */
extern enum reg_class cr16_regno_reg_class (int);
extern int cr16_hard_regno_mode_ok (int regno, enum machine_mode);

/* Passing function arguments.  */
extern int cr16_function_arg_regno_p (int);

#ifdef TREE_CODE
#ifdef RTX_CODE

extern void cr16_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);

#endif /* RTX_CODE.  */
#endif /* TREE_CODE.  */

/* Enumeration giving the various data models we support.  */
enum data_model_type
{
  DM_DEFAULT,		/* Default data model (in CR16C/C+ - up to 16M).  */
  DM_NEAR,		/* Near data model    (in CR16C/C+ - up to 1M).  */
  DM_FAR,		/* Far data model     (in CR16C+   - up to 4G)
			   (in CR16C    - up to 16M).  */
  ILLEGAL_DM		/* Illegal data model.  */
};

#ifdef RTX_CODE

/* Addressing Modes.  */
struct cr16_address
{
  rtx base;	 	/* Base register: Any register or register pair.  */
  rtx index;		/* Index register: If one is present.  */
  rtx disp;		/* Displacement or Absolute address.  */
  enum data_model_type data;	/* data ref type.  */
  int code;		/* Whether the address is code address. 
			   0 - data, 1 - code label, 2 - function label.  */
};

enum cr16_addrtype
{
  CR16_INVALID,
  CR16_REG_REL,
  CR16_REGP_REL,
  CR16_INDEX_REGP_REL,
  CR16_ABSOLUTE
};

extern void notice_update_cc PARAMS ((rtx));
extern int cr16_operand_bit_pos (int val, int bitval);
extern void cr16_decompose_const (rtx x, int *code,
				  enum data_model_type *data,
				  bool treat_as_const);
extern enum cr16_addrtype cr16_decompose_address (rtx addr,
						  struct cr16_address *out,
						  bool debug_print,
						  bool treat_as_const);
extern int cr16_const_double_ok (rtx op);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, enum machine_mode, rtx);


/* Prologue/Epilogue functions.  */
extern int cr16_initial_elimination_offset (int, int);
extern char *cr16_prepare_push_pop_string (int);
extern void cr16_expand_prologue (void);
extern void cr16_expand_epilogue (void);
extern const char *cr16_emit_add_sub_di (rtx *, enum rtx_code);
extern const char *cr16_emit_logical_di (rtx *, enum rtx_code);

#endif /* RTX_CODE.  */

/* Handling the "interrupt" attribute.  */
extern int cr16_interrupt_function_p (void);
extern bool cr16_is_data_model (enum data_model_type);

#endif /* Not GCC_CR16_PROTOS_H.  */ 
