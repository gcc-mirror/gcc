/* Multiply table generator for tile.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

/* This program creates a table used to compile multiply by constant
   efficiently.

   This program should be compiled by a c++ compiler.  If it's
   compiled with -DTILEPRO, it generates the multiply table for
   TILEPro; otherwise it generates the multiply table for TILE-Gx.
   Running the program produces the table in stdout.

   The program works by generating every possible combination of up to
   MAX_INSTRUCTIONS linear operators (such as add, sub, s2a, left
   shift) and computing the multiplier computed by those instructions.
   For example,

   s2a r2,r1,r1
   s2a r3,r2,r2

   multiplies r1 by 25.

   There are usually multiple instruction sequences to multiply by a
   given constant. This program keeps only the cheapest one.
   "Cheapest" is defined first by the minimum theoretical schedule
   length, and if those are equal then by the number of instructions,
   and if those are equal then by which instructions we "prefer"
   (e.g. because we think one might take infinitesimally less power
   than another, or simply because we arbitrarily pick one to be more
   canonical).

   Once this program has determined the best instruction sequence for
   each multiplier, it emits them in a table sorted by the multiplier
   so the user can binary-search it to look for a match.  The table is
   pruned based on various criteria to keep its sizes reasonable.  */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#define __STDC_LIMIT_MACROS
#include <stdint.h>

#include <map>

#ifdef TILEPRO

/* The string representing the architecture.  */
#define ARCH "tilepro"

/* The type for the multiplication.  */
typedef int MUL_TYPE;

#else

/* The string representing the architecture.  */
#define ARCH "tilegx"

/* The type for the multiplication.  */
typedef long long MUL_TYPE;

#endif

/* Longest instruction sequence this will produce. With the current
   stupid algorithm runtime grows very quickly with this number.  */
#define MAX_INSTRUCTIONS 4

/* Maximum number of subexpressions in the expression DAG being
   generated.  This is the same as the number of instructions, except
   that zero and the original register we'd like to multiply by a
   constant are also thrown into the mix.  */
#define MAX_SUBEXPRS (2 + MAX_INSTRUCTIONS)

#define MIN(x, y)  ((x) <= (y) ? (x) : (y))
#define MAX(x, y)  ((x) >= (y) ? (x) : (y))

/* For this program a unary op is one which has only one nonconstant
   operand.  So shift left by 5 is considered unary.  */
typedef MUL_TYPE (*unary_op_func) (MUL_TYPE);
typedef MUL_TYPE (*binary_op_func) (MUL_TYPE, MUL_TYPE);

/* This describes an operation like 'add two registers' or 'left-shift
   by 7'.

   We call something a 'unary' operator if it only takes in one
   register as input, even though it might have an implicit second
   constant operand.  Currently this is used for left-shift by
   constant.  */
class Operator
{
public:
  /* Construct for a binary operator.  */
  Operator (const char *pattern, const char *name, binary_op_func func,
	    int cost)
    : m_pattern (pattern), m_name (name), m_top_index (-1),
      m_unary_func (0), m_binary_func (func), m_cost (cost),
      m_rhs_if_unary (0)
  {
  }

  /* Construct for a unary operator.  */
  Operator (const char *pattern, const char *name, unary_op_func func,
	    int rhs_if_unary, int cost)
    : m_pattern (pattern), m_name (name), m_top_index (-1),
      m_unary_func (func), m_binary_func (0), m_cost (cost),
      m_rhs_if_unary (rhs_if_unary)
  {
  }

  bool is_unary () const
  {
    return m_binary_func == NULL;
  }

  /* Name of the pattern for this operation, e.g. CODE_FOR_addsi3.  */
  const char *m_pattern;

  /* Name of the opcode for this operation, e.g. add.  */
  const char *m_name;

  /* We don't have enough bits in our output representation to store
     the original insn_code value, so we store a compressed form
     instead.  These values are decoded back into insn_code via the
     machine-generated multiply_insn_seq_decode_opcode lookup
     table.  */
  int m_top_index;

  /* Unary operator to apply, or NULL if this is a binary operator.  */
  unary_op_func m_unary_func;

  /* Binary operator to apply, or NULL if this is a unary operator.  */
  binary_op_func m_binary_func;

  /* Function of how expensive we consider this operator. Higher is
     worse.  */
  int m_cost;

  /* the RHS value to write into the C file if unary; used for shift
     count.  */
  int m_rhs_if_unary;
};


/* An entry in an expression DAG.  */
class Expr
{
public:
  Expr () : m_op (NULL), m_lhs (0), m_rhs (0), m_produced_val (0),
    m_critical_path_length (0)
  {
  }

  /* The operator being applied to the operands.  */
  const Operator *m_op;

  /* The index of the left-hand operand in the array of subexpressions
     already computed.  */
  int m_lhs;

  /* For binary ops ,this is the index of the left-hand operand in the
     array of subexpressions already computed. For unary ops, it is
     specific to the op (e.g. it might hold a constant shift
     count).  */
  int m_rhs;

  /* The multiplier produced by this expression tree. For example, for
     the tree ((x << 5) + x), the value would be 33.  */
  MUL_TYPE m_produced_val;

  /* How far is this expression from the root, i.e. how many cycles
     minimum will it take to compute this?  */
  int m_critical_path_length;
};


/* Make function pointers for the various linear operators we can
   apply to compute a multiplicative value.  */

static MUL_TYPE
add (MUL_TYPE n1, MUL_TYPE n2)
{
  return n1 + n2;
}

static MUL_TYPE
sub (MUL_TYPE n1, MUL_TYPE n2)
{
  return n1 - n2;
}

static MUL_TYPE
s1a (MUL_TYPE n1, MUL_TYPE n2)
{
  return n1 * 2 + n2;
}

static MUL_TYPE
s2a (MUL_TYPE n1, MUL_TYPE n2)
{
  return n1 * 4 + n2;
}

static MUL_TYPE
s3a (MUL_TYPE n1, MUL_TYPE n2)
{
  return n1 * 8 + n2;
}

#define SHIFT(count)                            \
static MUL_TYPE                                 \
shift##count(MUL_TYPE n)                        \
{                                               \
  return n << (count);                          \
}

SHIFT (1);
SHIFT (2);
SHIFT (3);
SHIFT (4);
SHIFT (5);
SHIFT (6);
SHIFT (7);
SHIFT (8);
SHIFT (9);
SHIFT (10);
SHIFT (11);
SHIFT (12);
SHIFT (13);
SHIFT (14);
SHIFT (15);
SHIFT (16);
SHIFT (17);
SHIFT (18);
SHIFT (19);
SHIFT (20);
SHIFT (21);
SHIFT (22);
SHIFT (23);
SHIFT (24);
SHIFT (25);
SHIFT (26);
SHIFT (27);
SHIFT (28);
SHIFT (29);
SHIFT (30);
SHIFT (31);
#ifndef TILEPRO
SHIFT (32);
SHIFT (33);
SHIFT (34);
SHIFT (35);
SHIFT (36);
SHIFT (37);
SHIFT (38);
SHIFT (39);
SHIFT (40);
SHIFT (41);
SHIFT (42);
SHIFT (43);
SHIFT (44);
SHIFT (45);
SHIFT (46);
SHIFT (47);
SHIFT (48);
SHIFT (49);
SHIFT (50);
SHIFT (51);
SHIFT (52);
SHIFT (53);
SHIFT (54);
SHIFT (55);
SHIFT (56);
SHIFT (57);
SHIFT (58);
SHIFT (59);
SHIFT (60);
SHIFT (61);
SHIFT (62);
SHIFT (63);
#endif

#ifdef TILEPRO
static Operator ops[] = {
  Operator ("CODE_FOR_addsi3", "add", add, 1040),
  Operator ("CODE_FOR_subsi3", "sub", sub, 1041),
  Operator ("CODE_FOR_insn_s1a", "s1a", s1a, 1042),
  Operator ("CODE_FOR_insn_s2a", "s2a", s2a, 1043),
  Operator ("CODE_FOR_insn_s3a", "s3a", s3a, 1044),
  /* Note: shl by 1 is not necessary, since adding a value to itself
     produces the same result. But the architecture team thinks
     left-shift may use slightly less power, so we might as well
     prefer it.  */
  Operator ("CODE_FOR_ashlsi3", "shli", shift1, 1, 1001),
  Operator ("CODE_FOR_ashlsi3", "shli", shift2, 2, 1002),
  Operator ("CODE_FOR_ashlsi3", "shli", shift3, 3, 1003),
  Operator ("CODE_FOR_ashlsi3", "shli", shift4, 4, 1004),
  Operator ("CODE_FOR_ashlsi3", "shli", shift5, 5, 1005),
  Operator ("CODE_FOR_ashlsi3", "shli", shift6, 6, 1006),
  Operator ("CODE_FOR_ashlsi3", "shli", shift7, 7, 1007),
  Operator ("CODE_FOR_ashlsi3", "shli", shift8, 8, 1008),
  Operator ("CODE_FOR_ashlsi3", "shli", shift9, 9, 1009),
  Operator ("CODE_FOR_ashlsi3", "shli", shift10, 10, 1010),
  Operator ("CODE_FOR_ashlsi3", "shli", shift11, 11, 1011),
  Operator ("CODE_FOR_ashlsi3", "shli", shift12, 12, 1012),
  Operator ("CODE_FOR_ashlsi3", "shli", shift13, 13, 1013),
  Operator ("CODE_FOR_ashlsi3", "shli", shift14, 14, 1014),
  Operator ("CODE_FOR_ashlsi3", "shli", shift15, 15, 1015),
  Operator ("CODE_FOR_ashlsi3", "shli", shift16, 16, 1016),
  Operator ("CODE_FOR_ashlsi3", "shli", shift17, 17, 1017),
  Operator ("CODE_FOR_ashlsi3", "shli", shift18, 18, 1018),
  Operator ("CODE_FOR_ashlsi3", "shli", shift19, 19, 1019),
  Operator ("CODE_FOR_ashlsi3", "shli", shift20, 20, 1020),
  Operator ("CODE_FOR_ashlsi3", "shli", shift21, 21, 1021),
  Operator ("CODE_FOR_ashlsi3", "shli", shift22, 22, 1022),
  Operator ("CODE_FOR_ashlsi3", "shli", shift23, 23, 1023),
  Operator ("CODE_FOR_ashlsi3", "shli", shift24, 24, 1024),
  Operator ("CODE_FOR_ashlsi3", "shli", shift25, 25, 1025),
  Operator ("CODE_FOR_ashlsi3", "shli", shift26, 26, 1026),
  Operator ("CODE_FOR_ashlsi3", "shli", shift27, 27, 1027),
  Operator ("CODE_FOR_ashlsi3", "shli", shift28, 28, 1028),
  Operator ("CODE_FOR_ashlsi3", "shli", shift29, 29, 1029),
  Operator ("CODE_FOR_ashlsi3", "shli", shift30, 30, 1030),
  Operator ("CODE_FOR_ashlsi3", "shli", shift31, 31, 1031)
};
#else
static Operator ops[] = {
  Operator ("CODE_FOR_adddi3", "add", add, 1070),
  Operator ("CODE_FOR_subdi3", "sub", sub, 1071),
  Operator ("CODE_FOR_insn_shl1add", "shl1add", s1a, 1072),
  Operator ("CODE_FOR_insn_shl2add", "shl2add", s2a, 1073),
  Operator ("CODE_FOR_insn_shl3add", "shl3add", s3a, 1074),
  // Note: shl by 1 is not necessary, since adding a value to itself
  // produces the same result. But the architecture team thinks left-shift
  // may use slightly less power, so we might as well prefer it.
  Operator ("CODE_FOR_ashldi3", "shli", shift1, 1, 1001),
  Operator ("CODE_FOR_ashldi3", "shli", shift2, 2, 1002),
  Operator ("CODE_FOR_ashldi3", "shli", shift3, 3, 1003),
  Operator ("CODE_FOR_ashldi3", "shli", shift4, 4, 1004),
  Operator ("CODE_FOR_ashldi3", "shli", shift5, 5, 1005),
  Operator ("CODE_FOR_ashldi3", "shli", shift6, 6, 1006),
  Operator ("CODE_FOR_ashldi3", "shli", shift7, 7, 1007),
  Operator ("CODE_FOR_ashldi3", "shli", shift8, 8, 1008),
  Operator ("CODE_FOR_ashldi3", "shli", shift9, 9, 1009),
  Operator ("CODE_FOR_ashldi3", "shli", shift10, 10, 1010),
  Operator ("CODE_FOR_ashldi3", "shli", shift11, 11, 1011),
  Operator ("CODE_FOR_ashldi3", "shli", shift12, 12, 1012),
  Operator ("CODE_FOR_ashldi3", "shli", shift13, 13, 1013),
  Operator ("CODE_FOR_ashldi3", "shli", shift14, 14, 1014),
  Operator ("CODE_FOR_ashldi3", "shli", shift15, 15, 1015),
  Operator ("CODE_FOR_ashldi3", "shli", shift16, 16, 1016),
  Operator ("CODE_FOR_ashldi3", "shli", shift17, 17, 1017),
  Operator ("CODE_FOR_ashldi3", "shli", shift18, 18, 1018),
  Operator ("CODE_FOR_ashldi3", "shli", shift19, 19, 1019),
  Operator ("CODE_FOR_ashldi3", "shli", shift20, 20, 1020),
  Operator ("CODE_FOR_ashldi3", "shli", shift21, 21, 1021),
  Operator ("CODE_FOR_ashldi3", "shli", shift22, 22, 1022),
  Operator ("CODE_FOR_ashldi3", "shli", shift23, 23, 1023),
  Operator ("CODE_FOR_ashldi3", "shli", shift24, 24, 1024),
  Operator ("CODE_FOR_ashldi3", "shli", shift25, 25, 1025),
  Operator ("CODE_FOR_ashldi3", "shli", shift26, 26, 1026),
  Operator ("CODE_FOR_ashldi3", "shli", shift27, 27, 1027),
  Operator ("CODE_FOR_ashldi3", "shli", shift28, 28, 1028),
  Operator ("CODE_FOR_ashldi3", "shli", shift29, 29, 1029),
  Operator ("CODE_FOR_ashldi3", "shli", shift30, 30, 1030),
  Operator ("CODE_FOR_ashldi3", "shli", shift31, 31, 1031),
  Operator ("CODE_FOR_ashldi3", "shli", shift32, 32, 1032),
  Operator ("CODE_FOR_ashldi3", "shli", shift33, 33, 1033),
  Operator ("CODE_FOR_ashldi3", "shli", shift34, 34, 1034),
  Operator ("CODE_FOR_ashldi3", "shli", shift35, 35, 1035),
  Operator ("CODE_FOR_ashldi3", "shli", shift36, 36, 1036),
  Operator ("CODE_FOR_ashldi3", "shli", shift37, 37, 1037),
  Operator ("CODE_FOR_ashldi3", "shli", shift38, 38, 1038),
  Operator ("CODE_FOR_ashldi3", "shli", shift39, 39, 1039),
  Operator ("CODE_FOR_ashldi3", "shli", shift40, 40, 1040),
  Operator ("CODE_FOR_ashldi3", "shli", shift41, 41, 1041),
  Operator ("CODE_FOR_ashldi3", "shli", shift42, 42, 1042),
  Operator ("CODE_FOR_ashldi3", "shli", shift43, 43, 1043),
  Operator ("CODE_FOR_ashldi3", "shli", shift44, 44, 1044),
  Operator ("CODE_FOR_ashldi3", "shli", shift45, 45, 1045),
  Operator ("CODE_FOR_ashldi3", "shli", shift46, 46, 1046),
  Operator ("CODE_FOR_ashldi3", "shli", shift47, 47, 1047),
  Operator ("CODE_FOR_ashldi3", "shli", shift48, 48, 1048),
  Operator ("CODE_FOR_ashldi3", "shli", shift49, 49, 1049),
  Operator ("CODE_FOR_ashldi3", "shli", shift50, 50, 1050),
  Operator ("CODE_FOR_ashldi3", "shli", shift51, 51, 1051),
  Operator ("CODE_FOR_ashldi3", "shli", shift52, 52, 1052),
  Operator ("CODE_FOR_ashldi3", "shli", shift53, 53, 1053),
  Operator ("CODE_FOR_ashldi3", "shli", shift54, 54, 1054),
  Operator ("CODE_FOR_ashldi3", "shli", shift55, 55, 1055),
  Operator ("CODE_FOR_ashldi3", "shli", shift56, 56, 1056),
  Operator ("CODE_FOR_ashldi3", "shli", shift57, 57, 1057),
  Operator ("CODE_FOR_ashldi3", "shli", shift58, 58, 1058),
  Operator ("CODE_FOR_ashldi3", "shli", shift59, 59, 1059),
  Operator ("CODE_FOR_ashldi3", "shli", shift60, 60, 1060),
  Operator ("CODE_FOR_ashldi3", "shli", shift61, 61, 1061),
  Operator ("CODE_FOR_ashldi3", "shli", shift62, 62, 1062),
  Operator ("CODE_FOR_ashldi3", "shli", shift63, 63, 1063)
};
#endif

/* An ExpressionTree is an expression DAG.  */
class ExpressionTree
{
public:
  ExpressionTree () : m_num_vals (2)
  {
    m_exprs[0].m_produced_val = 0;
    m_exprs[1].m_produced_val = 1;
  }

  void copy_technique_from (ExpressionTree * other)
  {
    /* TODO: write this; other can compute the same products with less
       cost.  We update this ExpressionTree object because some int is
       already mapped to it.  */
  }

  int m_num_vals;
  Expr m_exprs[MAX_SUBEXPRS];

  int cost () const
  {
    int cost = 0;
    for (int j = 2; j < m_num_vals; j++)
        cost += m_exprs[j].m_op->m_cost;
      return cost + m_exprs[m_num_vals - 1].m_critical_path_length * 1000000;
  }
};


typedef std::map<MUL_TYPE, ExpressionTree *> ExpressionTreeMap;


static void
find_sequences (ExpressionTree &s, ExpressionTreeMap &best_solution)
{
  /* Don't look more if we can't add any new values to the expression
     tree.  */
  const int num_vals = s.m_num_vals;
  if (num_vals == MAX_SUBEXPRS)
    return;

  /* Grow array to make room for new values added as we loop.  */
  s.m_num_vals = num_vals + 1;

  const Operator *const prev_op = s.m_exprs[num_vals - 1].m_op;
  const int prev_top_index = (prev_op != NULL) ? prev_op->m_top_index : -1;

  for (size_t f = 0; f < sizeof ops / sizeof ops[0]; f++)
    {
      const Operator *const op = &ops[f];

      for (int i = 0; i < num_vals; i++)
	{
	  /* Only allow zero as the first operand to sub, otherwise
	     it is useless.  */
	  if (i == 0 && op->m_binary_func != sub)
	    continue;

	  /* Unary ops don't actually use the second operand, so as a
	     big hack we trick it into only looping once in the inner
	     loop.  */
	  const int j_end = op->is_unary () ? 2 : num_vals;

	  /* We never allow zero as the second operand, as it is
	     always useless.  */
	  for (int j = 1; j < j_end; j++)
	    {
	      /* Does this expression use the immediately previous
		 expression?  */
	      const bool uses_prev_value =
		(i == num_vals - 1
		 || (!op->is_unary () && j == num_vals - 1));

	      if (!uses_prev_value)
		{
		  /* For search efficiency, prune redundant
		     instruction orderings.

		     This op does not take the immediately preceding
		     value as input, which means we could have done it
		     in the previous slot. If its opcode is less than
		     the previous instruction's, we'll declare this
		     instruction order non-canonical and not pursue
		     this branch of the search.  */
		  if (op->m_top_index < prev_top_index)
		    continue;
		}

	      MUL_TYPE n;
	      if (op->is_unary ())
		{
		  n = op->m_unary_func (s.m_exprs[i].m_produced_val);
		}
	      else
		{
		  n = op->m_binary_func (s.m_exprs[i].m_produced_val,
					 s.m_exprs[j].m_produced_val);
		}

	      bool duplicate = false;
	      for (int k = 0; k < num_vals; k++)
		if (n == s.m_exprs[j].m_produced_val)
		  {
		    duplicate = true;
		    break;
		  }

	      if (duplicate)
		continue;

	      /* See if we found the best solution for n.  */
	      Expr *e = &s.m_exprs[num_vals];
	      e->m_op = op;
	      e->m_lhs = i;
	      e->m_rhs = op->is_unary () ? op->m_rhs_if_unary : j;
	      e->m_produced_val = n;
	      e->m_critical_path_length =
		1 + MAX (s.m_exprs[i].m_critical_path_length,
			 s.m_exprs[j].m_critical_path_length);

	      ExpressionTreeMap::iterator best (best_solution.find (n));
	      if (best == best_solution.end ()
		  || (*best).second->cost () > s.cost ())
		best_solution[n] = new ExpressionTree (s);

	      /* Recurse and find more.  */
	      find_sequences (s, best_solution);
	    }
	}
    }

  /* Restore old size.  */
  s.m_num_vals = num_vals;
}


/* For each insn_code used by an operator, choose a compact number so
   it takes less space in the output data structure. This prints out a
   lookup table used to map the compactified number back to an
   insn_code.  */
static void
create_insn_code_compression_table ()
{
  int next_index = 1;

  /* Entry 0 must hold CODE_FOR_nothing to mean "end of array".  */
  printf ("const enum insn_code %s_multiply_insn_seq_decode_opcode[] = {\n"
	  "  CODE_FOR_nothing /* must be first */ ", ARCH);

  for (size_t i = 0; i < sizeof ops / sizeof ops[0]; i++)
    {
      Operator *op = &ops[i];
      int index = -1;

      /* See if some previous Operator was using the same insn_code.
	 If so, reuse its table entry.  */
      for (size_t j = 0; j < i; j++)
	{
	  Operator *old = &ops[j];
	  if (strcmp (old->m_pattern, op->m_pattern) == 0)
	    {
	      index = old->m_top_index;
	      break;
	    }
	}

      if (index == -1)
	{
	  /* We need to make a new entry in the table.  */
	  printf (",\n  %s", op->m_pattern);
	  index = next_index++;
	}

      op->m_top_index = index;
    }

  printf ("\n};\n\n");
}


/* These are constants we've seen in code, that we want to keep table
   entries for.  */
static int multiply_constants_used[] = {
  -11796480,
  -27439,
  -25148,
  -22820,
  -21709,
  -20995,
  -20284,
  -20239,
  -19595,
  -19447,
  -19183,
  -19165,
  -18730,
  -17828,
  -17799,
  -17237,
  -17036,
  -16549,
  -16423,
  -16294,
  -16244,
  -16069,
  -15137,
  -15083,
  -15038,
  -14924,
  -14905,
  -14752,
  -14731,
  -14529,
  -14273,
  -14090,
  -14084,
  -14043,
  -13850,
  -13802,
  -13631,
  -13455,
  -13275,
  -12879,
  -12700,
  -12534,
  -12399,
  -12131,
  -12112,
  -12054,
  -12019,
  -11759,
  -11585,
  -11467,
  -11395,
  -11295,
  -11248,
  -11148,
  -11116,
  -11086,
  -11059,
  -11018,
  -10811,
  -10538,
  -10258,
  -10217,
  -10033,
  -9766,
  -9754,
  -9534,
  -9527,
  -9467,
  -9262,
  -9232,
  -9222,
  -9198,
  -9191,
  -9113,
  -8825,
  -8756,
  -8697,
  -8693,
  -8565,
  -8342,
  -8208,
  -8200,
  -8170,
  -8102,
  -7770,
  -7678,
  -7562,
  -7376,
  -7373,
  -7221,
  -7121,
  -6835,
  -6810,
  -6626,
  -6581,
  -6461,
  -6278,
  -6263,
  -6163,
  -6029,
  -5816,
  -5540,
  -5461,
  -5384,
  -5329,
  -4985,
  -4926,
  -4815,
  -4788,
  -4758,
  -4433,
  -4229,
  -4209,
  -4176,
  -4104,
  -4095,
  -4078,
  -3941,
  -3818,
  -3600,
  -3474,
  -3314,
  -3264,
  -3196,
  -3072,
  -2912,
  -2836,
  -2773,
  -2269,
  -2184,
  -2100,
  -1730,
  -1512,
  -1500,
  -1396,
  -1344,
  -1312,
  -1297,
  -1059,
  -1058,
  1027,
  1049,
  1059,
  1100,
  1104,
  1108,
  1136,
  1200,
  1204,
  1242,
  1292,
  1304,
  1312,
  1320,
  1336,
  1344,
  1348,
  1360,
  1364,
  1395,
  1448,
  1460,
  1461,
  1472,
  1488,
  1500,
  1512,
  1568,
  1576,
  1649,
  1664,
  1684,
  1696,
  1744,
  1812,
  1822,
  1884,
  1963,
  1978,
  2000,
  2012,
  2014,
  2037,
  2039,
  2100,
  2139,
  2144,
  2184,
  2237,
  2260,
  2320,
  2408,
  2446,
  2447,
  2499,
  2531,
  2578,
  2592,
  2611,
  2633,
  2704,
  2730,
  2773,
  2880,
  2896,
  2998,
  3000,
  3001,
  3021,
  3079,
  3112,
  3150,
  3179,
  3192,
  3240,
  3264,
  3271,
  3283,
  3328,
  3363,
  3367,
  3453,
  3529,
  3570,
  3580,
  3600,
  3624,
  3707,
  3783,
  3826,
  3897,
  3941,
  3962,
  3989,
  4000,
  4025,
  4073,
  4093,
  4099,
  4108,
  4184,
  4209,
  4369,
  4376,
  4416,
  4433,
  4434,
  4482,
  4582,
  4712,
  4717,
  4813,
  4815,
  4864,
  5000,
  5027,
  5040,
  5091,
  5195,
  5243,
  5260,
  5285,
  5329,
  5331,
  5350,
  5361,
  5387,
  5461,
  5492,
  5529,
  5573,
  5793,
  5819,
  5915,
  5946,
  5992,
  6000,
  6164,
  6205,
  6262,
  6263,
  6269,
  6270,
  6387,
  6400,
  6406,
  6476,
  6541,
  6565,
  6568,
  6626,
  6656,
  6732,
  6810,
  6817,
  6859,
  7040,
  7053,
  7141,
  7169,
  7221,
  7223,
  7274,
  7282,
  7350,
  7369,
  7373,
  7442,
  7447,
  7471,
  7518,
  7542,
  7566,
  7587,
  7663,
  7678,
  7682,
  7748,
  7752,
  7791,
  8000,
  8026,
  8048,
  8170,
  8203,
  8204,
  8290,
  8368,
  8520,
  8640,
  8666,
  8672,
  8697,
  8716,
  8728,
  8756,
  8820,
  8875,
  8918,
  8956,
  9058,
  9154,
  9175,
  9191,
  9217,
  9262,
  9321,
  9373,
  9434,
  9465,
  9514,
  9534,
  9633,
  9746,
  9810,
  9850,
  9947,
  9973,
  10000,
  10009,
  10033,
  10055,
  10217,
  10248,
  10298,
  10310,
  10323,
  10368,
  10438,
  10456,
  10486,
  10538,
  10664,
  10695,
  10700,
  10703,
  10832,
  10887,
  10935,
  10958,
  11018,
  11059,
  11061,
  11086,
  11116,
  11148,
  11190,
  11249,
  11314,
  11332,
  11363,
  11409,
  11415,
  11443,
  11467,
  11512,
  11522,
  11529,
  11585,
  11759,
  11768,
  11795,
  11893,
  11997,
  12131,
  12299,
  12536,
  12543,
  12893,
  12945,
  12998,
  13109,
  13213,
  13685,
  13930,
  14023,
  14024,
  14271,
  14564,
  14647,
  15326,
  15850,
  15855,
  15929,
  16000,
  16154,
  16496,
  16807,
  16819,
  16984,
  17203,
  17223,
  17333,
  17760,
  17799,
  17837,
  18029,
  18068,
  18336,
  18515,
  19595,
  20017,
  20131,
  20862,
  20995,
  21709,
  22554,
  25000,
  25172,
  25600,
  25733,
  27439,
  38470,
  46802,
  50000,
  11796480,
  16843009,
  23592960,
};


const int num_mult_constants_used =
  (int)(sizeof multiply_constants_used
	/ sizeof multiply_constants_used[0]);


#define XSIZE (sizeof multiply_constants_used / \
	       sizeof multiply_constants_used[0] + 32) / 32
unsigned multiply_constants_avail[XSIZE];
#undef XSIZE


/* bsearch helper function.  */
static int
compare_constants (const void *key, const void *t)
{
  return (*(int*)key) - *((int*)t);
}


static int *
find_mult_constants_used (int multiplier)
{
  return (int *) bsearch (&multiplier, multiply_constants_used,
			  num_mult_constants_used,
			  sizeof multiply_constants_used[0],
			  compare_constants);
}


int num_ops (ExpressionTree *s)
{
  int n = 0;
  for (int i = 0; i < s->m_num_vals; i++)
    {
      Expr *e = &s->m_exprs[i];
      if (e->m_op != NULL)
	n++;
    }
  return n;
}


#ifdef TILEPRO
bool
tilepro_emit (int multiplier, int num_ops)
{
  int abs_multiplier = (multiplier >= 0) ? multiplier : -multiplier;

  /* Keep constants in range [-1024, 1024].  */
  if (abs_multiplier <= 1024)
    return true;

  /* Keep constants near powers of two.  */
  int prev_pow2 = 1 << (31 - __builtin_clz (abs_multiplier));
  int next_pow2 = prev_pow2 << 1;

  if ((abs_multiplier - prev_pow2 <= 10)
      || (next_pow2 - abs_multiplier <= 10))
    return true;

  /* Keep constants near powers of ten.  */
  {
    long long j = 1;
    long long prev_pow10;
    long long next_pow10;

    while (((j * 10) < abs_multiplier)
	   && (j < (j * 10)))
      j = j * 10;

    prev_pow10 = j;
    next_pow10 = j * 10;

    if ((abs_multiplier - prev_pow10 <= 10)
	|| (next_pow10 - abs_multiplier <= 10))
      return true;
  }

  /* Keep short sequences that have two or fewer ops.  */
  if (num_ops <= 2)
    return true;

  /* Keep constants that are mostly 0's or mostly 1's.  */
  if (__builtin_popcount (multiplier) <= 2 ||
      __builtin_popcount (multiplier) >= 30)
    return true;

  /* Keep constants seen in actual code.  */
  if ((find_mult_constants_used (multiplier)))
    return true;

  return false;
}
#else
bool
tilegx_emit (long long multiplier, int num_ops)
{
  long long abs_multiplier = (multiplier >= 0) ? multiplier : - multiplier;

  /* Keep constants in range [-1024, 1024].  */
  if (abs_multiplier <= 1024)
    return true;

  /* Otherwise exclude sequences with four ops.  */
  if (num_ops > 3)
    return false;

  /* Keep constants near powers of two.  */
  {
    unsigned long long prev_pow2 =
      1LL << (63 - __builtin_clzll (abs_multiplier));
    unsigned long long next_pow2 = prev_pow2 << 1;

    /* handle overflow case. */
    if (next_pow2 == 0)
      {
	if (prev_pow2 - abs_multiplier <= 10)
	  return true;
      }
    else if ((abs_multiplier - prev_pow2 <= 10)
	     || (next_pow2 - abs_multiplier <= 10))
      return true;
  }

  /* Keep constants near powers of ten.  */
  {
    long long j = 1;
    long long prev_pow10;
    long long next_pow10;

    while (((j * 10) < abs_multiplier)
	   && (j < (INTMAX_MAX / 10)))
      j = j * 10;

    prev_pow10 = j;
    next_pow10 = (j > (INTMAX_MAX / 10)) ? 0 : j * 10;

    if ((abs_multiplier - prev_pow10 <= 100)
	|| (next_pow10
	    && (next_pow10 - abs_multiplier <= 100)))
      return true;
  }

  if (num_ops <= 2)
    return true;

  /* Keep constants that are mostly 0's or mostly 1's.  */
  if (__builtin_popcountll (multiplier) <= 2 ||
      __builtin_popcountll (multiplier) >= 62)
    return true;

  /* Keep constants seen in actual code.  */
  if (find_mult_constants_used (multiplier))
    return true;

  return false;
}
#endif


int
main ()
{
  ExpressionTreeMap best_solution;
  ExpressionTree s;

#ifdef TILEPRO
  printf ("/* Constant multiply table for TILEPro.\n");
#else
  printf ("/* Constant multiply table for TILE-Gx.\n");
#endif
  printf ("   Copyright (C) 2011-2019 Free Software Foundation, Inc.\n");
  printf ("   Contributed by Walter Lee (walt@tilera.com)\n");
  printf ("\n");
  printf ("   This file is part of GCC.\n");
  printf ("\n");
  printf ("   GCC is free software; you can redistribute it and/or modify it\n");
  printf ("   under the terms of the GNU General Public License as published\n");
  printf ("   by the Free Software Foundation; either version 3, or (at your\n");
  printf ("   option) any later version.\n");
  printf ("\n");
  printf ("   GCC is distributed in the hope that it will be useful, but WITHOUT\n");
  printf ("   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n");
  printf ("   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public\n");
  printf ("   License for more details.\n");
  printf ("\n");
  printf ("   You should have received a copy of the GNU General Public License\n");
  printf ("   along with GCC; see the file COPYING3.  If not see\n");
  printf ("   <http://www.gnu.org/licenses/>.  */\n");
  printf ("\n");
  printf ("/* Note this file is auto-generated from gen-mul-tables.cc.\n");
  printf ("   Make any required changes there.  */\n");
  printf ("\n");
  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"coretypes.h\"\n");
  printf ("#include \"backend.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"expmed.h\"\n");
  printf ("#include \"%s-multiply.h\"\n\n", ARCH);
  create_insn_code_compression_table ();

  /* Try all combinations of operators and see what constants we
     produce.  For each possible constant, record the most efficient
     way to generate it.  */
  find_sequences (s, best_solution);

  printf ("const struct %s_multiply_insn_seq "
	  "%s_multiply_insn_seq_table[] = {\n",
	  ARCH, ARCH);

  const char *comma_separator = "";

  ExpressionTreeMap::iterator i (best_solution.begin ());
  for (; i != best_solution.end (); ++i)
    {
      ExpressionTree *s = (*i).second;
      const MUL_TYPE n = (*i).first;

      if (n == 0 || n == 1)
	{
	  /* Both of these require zero operations, so these entries
	     are bogus and should never be used.  */
	  continue;
	}

      /* Prune the list of entries to keep the table to a reasonable
	 size.  */
#ifdef TILEPRO
      if (!tilepro_emit (n, num_ops (s)))
	continue;
#else
      if (!tilegx_emit (n, num_ops (s)))
	continue;
#endif

      printf ("%s", comma_separator);

#ifdef TILEPRO
      const MUL_TYPE int_min = INT32_MIN;
#else
      const MUL_TYPE int_min = INT64_MIN;
#endif
      if (n == int_min)
	{
	  /* Handle C's woeful lack of unary negation. Without this,
	     printing out INT_MIN in decimal will yield an unsigned
	     int which could generate a compiler warning.  */
#ifdef TILEPRO
	  printf ("  {%d - 1 /* 0x%x */ ,\n   {", n + 1,
		  (unsigned) n);
#else
	  printf ("  {%lldll - 1 /* 0x%llx */ ,\n   {", n + 1,
		  (unsigned MUL_TYPE) n);
#endif
	}
      else
	{
#ifdef TILEPRO
	  printf ("  {%d /* 0x%x */ ,\n   {", n, (unsigned) n);
#else
	  printf ("  {%lldll /* 0x%llx */ ,\n   {", n, (unsigned MUL_TYPE) n);
#endif
	}

      bool first = true;
      for (int j = 0; j < s->m_num_vals; j++)
	{
	  Expr *e = &s->m_exprs[j];

	  const Operator *op = e->m_op;
	  if (op == NULL)
	    continue;

	  char buf[1024];
	  snprintf (buf, sizeof buf, "%s{%d, %d, %d}%s",
		    first ? "" : "    ",
		    op->m_top_index,
		    e->m_lhs, e->m_rhs, (j + 1) == s->m_num_vals ? "}" : ",");

	  char opnd0[10];
	  if (e->m_lhs)
	    snprintf (opnd0, sizeof opnd0, "r%d", e->m_lhs);
	  else
	    snprintf (opnd0, sizeof opnd0, "zero");

	  printf ("%s\t\t\t/* %s r%d, %s, %s%d */\n",
		  buf, op->m_name, j, opnd0,
		  op->is_unary () ? "" : "r", e->m_rhs);

	  first = false;
	}
      printf ("   }");
      comma_separator = ",\n";
    }

  printf ("\n};\n\n");
  printf ("const int %s_multiply_insn_seq_table_size =\n"
	  "  (int) (sizeof %s_multiply_insn_seq_table\n"
	  "         / sizeof %s_multiply_insn_seq_table[0]);\n",
	  ARCH, ARCH, ARCH);

  return EXIT_SUCCESS;
}
