/* Declarations for rtx-reader support for gen* routines.
   Copyright (C) 2000, 2002, 2003, 2004, 2007, 2008, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GENSUPPORT_H
#define GCC_GENSUPPORT_H

struct obstack;
extern struct obstack *rtl_obstack;

extern bool init_rtx_reader_args_cb (int, char **, bool (*)(const char *));
extern bool init_rtx_reader_args (int, char **);
extern rtx read_md_rtx (int *, int *);

/* Set this to 0 to disable automatic elision of insn patterns which
   can never be used in this configuration.  See genconditions.c.
   Must be set before calling init_md_reader.  */
extern int insn_elision;

/* If the C test passed as the argument can be evaluated at compile
   time, return its truth value; else return -1.  The test must have
   appeared somewhere in the machine description when genconditions
   was run.  */
extern int maybe_eval_c_test (const char *);

/* Add an entry to the table of conditions.  Used by genconditions and
   by read-rtl.c.  */
extern void add_c_test (const char *, int);

/* This structure is used internally by gensupport.c and genconditions.c.  */
struct c_test
{
  const char *expr;
  int value;
};

#ifdef __HASHTAB_H__
extern hashval_t hash_c_test (const void *);
extern int cmp_c_test (const void *, const void *);
extern void traverse_c_tests (htab_trav, void *);
#endif

/* Predicate handling: helper functions and data structures.  */

struct pred_data
{
  struct pred_data *next;	/* for iterating over the set of all preds */
  const char *name;		/* predicate name */
  bool special;			/* special handling of modes? */

  /* data used primarily by genpreds.c */
  const char *c_block;		/* C test block */
  rtx exp;			/* RTL test expression */

  /* data used primarily by genrecog.c */
  enum rtx_code singleton;	/* if pred takes only one code, that code */
  int num_codes;		/* number of codes accepted */
  bool allows_non_lvalue;	/* if pred allows non-lvalue expressions */
  bool allows_non_const;	/* if pred allows non-const expressions */
  bool codes[NUM_RTX_CODE];	/* set of codes accepted */
};

extern struct pred_data *first_predicate;
extern struct pred_data *lookup_predicate (const char *);
extern void add_predicate_code (struct pred_data *, enum rtx_code);
extern void add_predicate (struct pred_data *);

#define FOR_ALL_PREDICATES(p) for (p = first_predicate; p; p = p->next)

struct pattern_stats
{
  /* The largest match_operand, match_operator or match_parallel
     number found.  */
  int max_opno;

  /* The largest match_dup, match_op_dup or match_par_dup number found.  */
  int max_dup_opno;

  /* The largest match_scratch number found.  */
  int max_scratch_opno;

  /* The number of times match_dup, match_op_dup or match_par_dup appears
     in the pattern.  */
  int num_dups;

  /* The number of rtx arguments to the generator function.  */
  int num_generator_args;

  /* The number of rtx operands in an insn.  */
  int num_insn_operands;

  /* The number of operand variables that are needed.  */
  int num_operand_vars;
};

extern void get_pattern_stats (struct pattern_stats *ranges, rtvec vec);

#endif /* GCC_GENSUPPORT_H */
