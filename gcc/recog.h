/* Declarations for interface to insn recognizer and insn-output.c.
   Copyright (C) 1987, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

/* Random number that should be large enough for all purposes.  */
#define MAX_RECOG_ALTERNATIVES 30

/* Types of operands.  */
enum op_type {
  OP_IN,
  OP_OUT,
  OP_INOUT
};

struct operand_alternative
{
  /* Pointer to the beginning of the constraint string for this alternative,
     for easier access by alternative number.  */
  const char *constraint;

  /* The register class valid for this alternative (possibly NO_REGS).  */
  enum reg_class class;

  /* "Badness" of this alternative, computed from number of '?' and '!'
     characters in the constraint string.  */
  unsigned int reject;

  /* -1 if no matching constraint was found, or an operand number.  */
  int matches;
  /* The same information, but reversed: -1 if this operand is not
     matched by any other, or the operand number of the operand that
     matches this one.  */
  int matched;

  /* Nonzero if '&' was found in the constraint string.  */
  unsigned int earlyclobber:1;
  /* Nonzero if 'm' was found in the constraint string.  */
  unsigned int memory_ok:1;  
  /* Nonzero if 'o' was found in the constraint string.  */
  unsigned int offmem_ok:1;  
  /* Nonzero if 'V' was found in the constraint string.  */
  unsigned int nonoffmem_ok:1;
  /* Nonzero if '<' was found in the constraint string.  */
  unsigned int decmem_ok:1;
  /* Nonzero if '>' was found in the constraint string.  */
  unsigned int incmem_ok:1;
  /* Nonzero if 'X' was found in the constraint string, or if the constraint
     string for this alternative was empty.  */
  unsigned int anything_ok:1;
};


extern void init_recog			PROTO((void));
extern void init_recog_no_volatile	PROTO((void));
extern int recog_memoized		PROTO((rtx));
extern int check_asm_operands		PROTO((rtx));
extern int asm_operand_ok		PROTO((rtx, const char *));
extern int validate_change		PROTO((rtx, rtx *, rtx, int));
extern int apply_change_group		PROTO((void));
extern int num_validated_changes	PROTO((void));
extern void cancel_changes		PROTO((int));
extern int constrain_operands		PROTO((int));
extern int memory_address_p		PROTO((enum machine_mode, rtx));
extern int strict_memory_address_p	PROTO((enum machine_mode, rtx));
extern int validate_replace_rtx		PROTO((rtx, rtx, rtx));
extern void validate_replace_rtx_group	PROTO((rtx, rtx, rtx));
extern int validate_replace_src		PROTO((rtx, rtx, rtx));
#ifdef HAVE_cc0
extern int next_insn_tests_no_inequality PROTO ((rtx));
#endif
extern int reg_fits_class_p		PROTO((rtx, enum reg_class, int,
					       enum machine_mode));
extern rtx *find_single_use		PROTO((rtx, rtx, rtx *));

extern int general_operand		PROTO((rtx, enum machine_mode));
extern int address_operand		PROTO((rtx, enum machine_mode));
extern int register_operand		PROTO((rtx, enum machine_mode));
extern int scratch_operand		PROTO((rtx, enum machine_mode));
extern int immediate_operand		PROTO((rtx, enum machine_mode));
extern int const_int_operand		PROTO((rtx, enum machine_mode));
extern int const_double_operand		PROTO((rtx, enum machine_mode));
extern int nonimmediate_operand		PROTO((rtx, enum machine_mode));
extern int nonmemory_operand		PROTO((rtx, enum machine_mode));
extern int push_operand			PROTO((rtx, enum machine_mode));
extern int pop_operand			PROTO((rtx, enum machine_mode));
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
extern void extract_insn		PROTO((rtx));
extern void preprocess_constraints	PROTO((void));

/* Nonzero means volatile operands are recognized.  */
extern int volatile_ok;

/* Set by constrain_operands to the number of the alternative that
   matched.  */
extern int which_alternative;

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

/* The next variables are set up by extract_insn.  */

/* The number of operands of the insn.  */
extern int recog_n_operands;

/* The number of MATCH_DUPs in the insn.  */
extern int recog_n_dups;

/* The number of alternatives in the constraints for the insn.  */
extern int recog_n_alternatives;

/* Indexed by N, gives the mode of operand N.  */
extern enum machine_mode recog_operand_mode[];

/* Indexed by N, gives the constraint string for operand N.  */
extern const char *recog_constraints[];

/* Indexed by N, gives the type (in, out, inout) for operand N.  */
extern enum op_type recog_op_type[];

#ifndef REGISTER_CONSTRAINTS
/* Indexed by N, nonzero if operand N should be an address.  */
extern char recog_operand_address_p[];
#endif

/* Contains a vector of operand_alternative structures for every operand.
   Set up by preprocess_constraints.  */
extern struct operand_alternative recog_op_alt[MAX_RECOG_OPERANDS][MAX_RECOG_ALTERNATIVES];

/* Access the output function for CODE.  */

#define OUT_FCN(CODE) (*insn_outfun[(int) (CODE)])

/* Tables defined in insn-output.c that give information about
   each insn-code value.  */

/* These are vectors indexed by insn-code.  Details in genoutput.c.  */

extern const char *const insn_template[];

extern const char *(*const insn_outfun[]) PROTO ((rtx *, rtx));

extern const int insn_n_operands[];

extern const int insn_n_dups[];

/* Indexed by insn code number, gives # of constraint alternatives.  */

extern const int insn_n_alternatives[];

/* These are two-dimensional arrays indexed first by the insn-code
   and second by the operand number.  Details in genoutput.c.  */

#ifdef REGISTER_CONSTRAINTS  /* Avoid undef sym in certain broken linkers.  */
extern const char *const insn_operand_constraint[][MAX_RECOG_OPERANDS];
#endif

#ifndef REGISTER_CONSTRAINTS  /* Avoid undef sym in certain broken linkers.  */
extern const char insn_operand_address_p[][MAX_RECOG_OPERANDS];
#endif

extern const enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS];

extern const char insn_operand_strict_low[][MAX_RECOG_OPERANDS];

extern int (*const insn_operand_predicate[][MAX_RECOG_OPERANDS]) PROTO ((rtx, enum machine_mode));

extern const char * insn_name[];
