/* Declarations for interface to insn recognizer and insn-output.c.
   Copyright (C) 1987, 1996, 1997 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

#include "gansidecl.h"

extern void init_recog			PROTO((void));
extern void init_recog_no_volatile	PROTO((void));
extern int recog_memoized		PROTO((rtx));
extern int check_asm_operands		PROTO((rtx));
extern int validate_change		PROTO((rtx, rtx *, rtx, int));
extern int apply_change_group		PROTO((void));
extern int num_validated_changes	PROTO((void));
extern void cancel_changes		PROTO((int));
extern int constrain_operands		PROTO((int, int));
extern int memory_address_p		PROTO((enum machine_mode, rtx));
extern int strict_memory_address_p	PROTO((enum machine_mode, rtx));
extern int validate_replace_rtx		PROTO((rtx, rtx, rtx));
extern int reg_fits_class_p		PROTO((rtx, enum reg_class, int,
					       enum machine_mode));
extern rtx *find_single_use		PROTO((rtx, rtx, rtx *));

extern int general_operand		PROTO((rtx, enum machine_mode));
extern int address_operand		PROTO((rtx, enum machine_mode));
extern int register_operand		PROTO((rtx, enum machine_mode));
extern int scratch_operand		PROTO((rtx, enum machine_mode));
extern int immediate_operand		PROTO((rtx, enum machine_mode));
extern int const_int_operand		PROTO((rtx, enum machine_mode));
extern int cosnt_double_operand		PROTO((rtx, enum machine_mode));
extern int nonimmediate_operand		PROTO((rtx, enum machine_mode));
extern int nonmemory_operand		PROTO((rtx, enum machine_mode));
extern int push_operand			PROTO((rtx, enum machine_mode));
extern int memory_operand		PROTO((rtx, enum machine_mode));
extern int indirect_operand		PROTO((rtx, enum machine_mode));
extern int mode_independent_operand	PROTO((rtx, enum machine_mode));
extern int comparison_operator		PROTO((rtx, enum machine_mode));

extern int offsettable_memref_p		PROTO((rtx));
extern int offsettable_nonstrict_memref_p	PROTO((rtx));
extern int offsettable_address_p	PROTO((int, enum machine_mode, rtx));
extern int mode_dependent_address_p	PROTO((rtx));

extern int recog			PROTO((rtx, rtx, int *));
extern void add_clobbers		PROTO((rtx, int));
extern void insn_extract		PROTO((rtx));

/* Nonzero means volatile operands are recognized.  */
extern int volatile_ok;

/* The following vectors hold the results from insn_extract.  */

/* Indexed by N, gives value of operand N.  */
extern rtx recog_operand[];

/* Indexed by N, gives location where operand N was found.  */
extern rtx *recog_operand_loc[];

/* Indexed by N, gives location where the Nth duplicate-appearance of
   an operand was found.  This is something that matched MATCH_DUP.  */
extern rtx *recog_dup_loc[];

/* Indexed by N, gives the operand number that was duplicated in the
   Nth duplicate-appearance of an operand.  */
extern char recog_dup_num[];

/* Access the output function for CODE.  */

#define OUT_FCN(CODE) (*insn_outfun[(int) (CODE)])

/* Tables defined in insn-output.c that give information about
   each insn-code value.  */

/* These are vectors indexed by insn-code.  Details in genoutput.c.  */

extern char *const insn_template[];

extern char *(*const insn_outfun[]) ();

extern const int insn_n_operands[];

extern const int insn_n_dups[];

/* Indexed by insn code number, gives # of constraint alternatives.  */

extern const int insn_n_alternatives[];

/* These are two-dimensional arrays indexed first by the insn-code
   and second by the operand number.  Details in genoutput.c.  */

#ifdef REGISTER_CONSTRAINTS  /* Avoid undef sym in certain broken linkers.  */
extern char *const insn_operand_constraint[][MAX_RECOG_OPERANDS];
#endif

#ifndef REGISTER_CONSTRAINTS  /* Avoid undef sym in certain broken linkers.  */
extern const char insn_operand_address_p[][MAX_RECOG_OPERANDS];
#endif

extern const enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS];

extern const char insn_operand_strict_low[][MAX_RECOG_OPERANDS];

extern int (*const insn_operand_predicate[][MAX_RECOG_OPERANDS]) ();

extern char * insn_name[];
