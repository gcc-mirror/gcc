/* Declarations for interface to insn recognizer and insn-output.c.
   Copyright (C) 1987, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Random number that should be large enough for all purposes.  */
#define MAX_RECOG_ALTERNATIVES 30
#define recog_memoized(I) (INSN_CODE (I) >= 0 \
			   ? INSN_CODE (I) : recog_memoized_1 (I))

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
  /* Nonzero if 'p' was found in the constraint string.  */
  unsigned int is_address:1;
  /* Nonzero if 'X' was found in the constraint string, or if the constraint
     string for this alternative was empty.  */
  unsigned int anything_ok:1;
};


extern void init_recog (void);
extern void init_recog_no_volatile (void);
extern int recog_memoized_1 (rtx);
extern int check_asm_operands (rtx);
extern int asm_operand_ok (rtx, const char *);
extern int validate_change (rtx, rtx *, rtx, int);
extern int insn_invalid_p (rtx);
extern int apply_change_group (void);
extern int num_validated_changes (void);
extern void cancel_changes (int);
extern int constrain_operands (int);
extern int constrain_operands_cached (int);
extern int memory_address_p (enum machine_mode, rtx);
extern int strict_memory_address_p (enum machine_mode, rtx);
extern int validate_replace_rtx_subexp (rtx, rtx, rtx, rtx *);
extern int validate_replace_rtx (rtx, rtx, rtx);
extern void validate_replace_rtx_group (rtx, rtx, rtx);
extern int validate_replace_src (rtx, rtx, rtx);
extern void validate_replace_src_group (rtx, rtx, rtx);
extern int num_changes_pending (void);
#ifdef HAVE_cc0
extern int next_insn_tests_no_inequality (rtx);
#endif
extern int reg_fits_class_p (rtx, enum reg_class, int, enum machine_mode);
extern rtx *find_single_use (rtx, rtx, rtx *);

extern int general_operand (rtx, enum machine_mode);
extern int address_operand (rtx, enum machine_mode);
extern int register_operand (rtx, enum machine_mode);
extern int pmode_register_operand (rtx, enum machine_mode);
extern int scratch_operand (rtx, enum machine_mode);
extern int immediate_operand (rtx, enum machine_mode);
extern int const_int_operand (rtx, enum machine_mode);
extern int const_double_operand (rtx, enum machine_mode);
extern int nonimmediate_operand (rtx, enum machine_mode);
extern int nonmemory_operand (rtx, enum machine_mode);
extern int push_operand (rtx, enum machine_mode);
extern int pop_operand (rtx, enum machine_mode);
extern int memory_operand (rtx, enum machine_mode);
extern int indirect_operand (rtx, enum machine_mode);
extern int comparison_operator (rtx, enum machine_mode);

extern int offsettable_memref_p (rtx);
extern int offsettable_nonstrict_memref_p (rtx);
extern int offsettable_address_p (int, enum machine_mode, rtx);
extern int mode_dependent_address_p (rtx);

extern int recog (rtx, rtx, int *);
extern void add_clobbers (rtx, int);
extern int added_clobbers_hard_reg_p (int);
extern void insn_extract (rtx);
extern void extract_insn (rtx);
extern void extract_constrain_insn_cached (rtx);
extern void extract_insn_cached (rtx);
extern void preprocess_constraints (void);
extern rtx peep2_next_insn (int);
extern int peep2_regno_dead_p (int, int);
extern int peep2_reg_dead_p (int, rtx);
#ifdef CLEAR_HARD_REG_SET
extern rtx peep2_find_free_register (int, int, const char *,
				     enum machine_mode, HARD_REG_SET *);
#endif
extern void peephole2_optimize (FILE *);
extern rtx peephole2_insns (rtx, rtx, int *);

extern int store_data_bypass_p (rtx, rtx);
extern int if_test_bypass_p (rtx, rtx);

/* Nonzero means volatile operands are recognized.  */
extern int volatile_ok;

/* Set by constrain_operands to the number of the alternative that
   matched.  */
extern int which_alternative;

/* The following vectors hold the results from insn_extract.  */

struct recog_data
{
  /* It is very tempting to make the 5 operand related arrays into a
     structure and index on that.  However, to be source compatible
     with all of the existing md file insn constraints and output
     templates, we need `operand' as a flat array.  Without that
     member, making an array for the rest seems pointless.  */

  /* Gives value of operand N.  */
  rtx operand[MAX_RECOG_OPERANDS];

  /* Gives location where operand N was found.  */
  rtx *operand_loc[MAX_RECOG_OPERANDS];

  /* Gives the constraint string for operand N.  */
  const char *constraints[MAX_RECOG_OPERANDS];

  /* Gives the mode of operand N.  */
  enum machine_mode operand_mode[MAX_RECOG_OPERANDS];

  /* Gives the type (in, out, inout) for operand N.  */
  enum op_type operand_type[MAX_RECOG_OPERANDS];

  /* Gives location where the Nth duplicate-appearance of an operand
     was found.  This is something that matched MATCH_DUP.  */
  rtx *dup_loc[MAX_DUP_OPERANDS];

  /* Gives the operand number that was duplicated in the Nth
     duplicate-appearance of an operand.  */
  char dup_num[MAX_DUP_OPERANDS];

  /* ??? Note that these are `char' instead of `unsigned char' to (try to)
     avoid certain lossage from K&R C, wherein `unsigned char' default
     promotes to `unsigned int' instead of `int' as in ISO C.  As of 1999,
     the most common places to bootstrap from K&R C are SunOS and HPUX,
     both of which have signed characters by default.  The only other
     supported natives that have both K&R C and unsigned characters are
     ROMP and Irix 3, and neither have been seen for a while, but do
     continue to consider unsignedness when performing arithmetic inside
     a comparison.  */

  /* The number of operands of the insn.  */
  char n_operands;

  /* The number of MATCH_DUPs in the insn.  */
  char n_dups;

  /* The number of alternatives in the constraints for the insn.  */
  char n_alternatives;

  /* In case we are caching, hold insn data was generated for.  */
  rtx insn;
};

extern struct recog_data recog_data;

/* Contains a vector of operand_alternative structures for every operand.
   Set up by preprocess_constraints.  */
extern struct operand_alternative recog_op_alt[MAX_RECOG_OPERANDS][MAX_RECOG_ALTERNATIVES];

/* A table defined in insn-output.c that give information about
   each insn-code value.  */

typedef int (*insn_operand_predicate_fn) (rtx, enum machine_mode);
typedef const char * (*insn_output_fn) (rtx *, rtx);
typedef rtx (*insn_gen_fn) (rtx, ...);

struct insn_operand_data
{
  const insn_operand_predicate_fn predicate;

  const char *const constraint;

  ENUM_BITFIELD(machine_mode) const mode : 16;

  const char strict_low;

  const char eliminable;
};

/* Legal values for insn_data.output_format.  Indicate what type of data
   is stored in insn_data.output.  */
#define INSN_OUTPUT_FORMAT_NONE		0	/* abort */
#define INSN_OUTPUT_FORMAT_SINGLE	1	/* const char * */
#define INSN_OUTPUT_FORMAT_MULTI	2	/* const char * const * */
#define INSN_OUTPUT_FORMAT_FUNCTION	3	/* const char * (*)(...) */

struct insn_data
{
  const char *const name;
#if HAVE_DESIGNATED_INITIALIZERS
  union {
    const char *single;
    const char *const *multi;
    insn_output_fn function;
  } output;
#else
  struct {
    const char *single;
    const char *const *multi;
    insn_output_fn function;
  } output;
#endif
  const insn_gen_fn genfun;
  const struct insn_operand_data *const operand;

  const char n_operands;
  const char n_dups;
  const char n_alternatives;
  const char output_format;
};

extern const struct insn_data insn_data[];
