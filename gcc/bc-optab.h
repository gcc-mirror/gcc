/* Bytecode token definitions for GNU C-compiler.
   Copyright (C) 1993 Free Software Foundation, Inc.

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


extern void bc_expand_conversion ();
extern void bc_expand_truth_conversion ();
extern void bc_expand_binary_operation ();
extern void bc_expand_unary_operation ();

struct binary_operator
{
  enum bytecode_opcode opcode;
  enum typecode result;
  enum typecode arg0;
  enum typecode arg1;
};

extern struct binary_operator optab_plus_expr[];
extern struct binary_operator optab_minus_expr[];
extern struct binary_operator optab_mult_expr[];
extern struct binary_operator optab_trunc_div_expr[];
extern struct binary_operator optab_trunc_mod_expr[];
extern struct binary_operator optab_rdiv_expr[];
extern struct binary_operator optab_bit_and_expr[];
extern struct binary_operator optab_bit_ior_expr[];
extern struct binary_operator optab_bit_xor_expr[];
extern struct binary_operator optab_lshift_expr[];
extern struct binary_operator optab_rshift_expr[];
extern struct binary_operator optab_truth_and_expr[];
extern struct binary_operator optab_truth_or_expr[];
extern struct binary_operator optab_lt_expr[];
extern struct binary_operator optab_le_expr[];
extern struct binary_operator optab_ge_expr[];
extern struct binary_operator optab_gt_expr[];
extern struct binary_operator optab_eq_expr[];
extern struct binary_operator optab_ne_expr[];

struct unary_operator
{
  enum bytecode_opcode opcode;
  enum typecode result;
  enum typecode arg0;
};

extern struct unary_operator optab_negate_expr[];
extern struct unary_operator optab_bit_not_expr[];
extern struct unary_operator optab_truth_not_expr[];

struct increment_operator
{
  enum bytecode_opcode opcode;
  enum typecode arg;
};

extern struct increment_operator optab_predecrement_expr[];
extern struct increment_operator optab_preincrement_expr[];
extern struct increment_operator optab_postdecrement_expr[];
extern struct increment_operator optab_postincrement_expr[];
