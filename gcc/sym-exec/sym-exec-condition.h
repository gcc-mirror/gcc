/* Everything defined here is used for representing conditions for bits
   and their status.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Matevos Mehrabyan <matevosmehrabyan@gmail.com>

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


#ifndef SYM_EXEC_CONDITION_H
#define SYM_EXEC_CONDITION_H

#include "sym-exec-expression.h"

/* Enum representing condition status.  */

enum condition_status {
  CS_NO_COND,
  CS_TRUE,
  CS_FALSE,
  CS_SYM
};

/* Class used for describing and storing condition for a single bit.  */

class bit_condition : public bit_expression {
 private:
  /* Condition's code.  */
  tree_code m_code;

  /* Prints the condition's sign.  */
  void print_expr_sign ();

 public:
  /* Constructor where the first argument is the bit left to the condition sign,
     the second argument is the bit right to the condition sign and the third
     argument is the code of the condition.  */
  bit_condition (value_bit *left, value_bit *right, tree_code type);

  /* Copy constructor.  */
  bit_condition (const bit_condition &expr);

  /* Returns the condition's code.  */
  tree_code get_code () const;

  /* Returns a copy of the condition.  */
  value_bit *copy () const;
};

#endif /* SYM_EXEC_CONDITION_H.  */
