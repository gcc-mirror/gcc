/* Header for constant multiple table for TILEPro.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

#ifndef GCC_TILEPRO_MULTIPLY_H
#define GCC_TILEPRO_MULTIPLY_H

/* A node of a tilepro_multiply_insn_seq, corresponding to a single
   machine instruction such as 'add', 's1a', or an shl by a
   constant.  */
struct tilepro_multiply_insn_seq_entry
{
  /* Which operation this node performs (e.g. an add or sub).  Don't
     use this directly, call get_opcode() table to get a
     insn_code.  */
  unsigned char compressed_opcode;

  /* The left-hand side of this expression tree.
     If equal to 0, it refers to 'zero'.
     If equal to 1, it refers to the original input to the multiply
     operation.
     Otherwise, subtract two and it is an index into the containing
     tilepro_multiply_insn_seq's 'op' array. Since it can only point
     to some value that has already been computed it will always point
     to an earlier entry in the array.  */
  unsigned char lhs;

  /* This is like lhs, but for the right-hand side. However, for shift
     opcodes this is a shift count rather than an operand index.  */
  unsigned char rhs;
};

/* Maximum size of op array.  */
#define tilepro_multiply_insn_seq_MAX_OPERATIONS 4

/* This defines a DAG describing how to multiply by a constant in
   terms of one or more machine instructions.  */
struct tilepro_multiply_insn_seq
{
  /* The constant factor by which this expression tree multiplies its
     input.  */
  int multiplier;

  /* The nodes of the parse tree. These are ordered so that
     instructions can be emitted in the same order that they appear in
     this array.  Entry entry in this array can only refer to earlier
     entries in the array.  */
  struct tilepro_multiply_insn_seq_entry
    op[tilepro_multiply_insn_seq_MAX_OPERATIONS];

};

/* A mapping from the compressed opcode to the corresponding enum
   insn_code.  */
extern const enum insn_code tilepro_multiply_insn_seq_decode_opcode[];

/* Table mapping constant int multipliers to an expression tree that
   efficiently performs that multiplication.  This is sorted by its
   'multiplier' field so a binary search can look for matches.  */
extern const struct tilepro_multiply_insn_seq
  tilepro_multiply_insn_seq_table[];

/* The number of elements in multiply_insn_seq_table.  */
extern const int tilepro_multiply_insn_seq_table_size;

#endif /* !GCC_TILEPRO_MULTIPLY_H */
