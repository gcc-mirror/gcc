/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

#ifndef SYM_EXEC_EXPRESSION_H
#define SYM_EXEC_EXPRESSION_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "hwint.h"
#include "gimple-pretty-print.h"
#include "is-a.h"
#include "vec.h"
#include "hash-map.h"
#include "hash-set.h"
#include "stddef.h"

/* Enum used for identifying the class of the bit.  */

enum value_type {
  SYMBOLIC_BIT,
  BIT,
  BIT_XOR_EXPRESSION,
  BIT_AND_EXPRESSION,
  BIT_OR_EXPRESSION,
  BIT_COMPLEMENT_EXPRESSION,
  SHIFT_RIGHT_EXPRESSION,
  SHIFT_LEFT_EXPRESSION,
  ADD_EXPRESSION,
  SUB_EXPRESSION,
  BIT_CONDITION
};


/* Base class for single bit value.  */

class value_bit {
 protected:
  /* This will help us to understand where is moved the bit
     from its initial position.  */
  const size_t m_index;

  /* Type of the bit.  Used by type checkers.  */
  value_type m_type;

 public:

  /* Default constructor.  Sets m_index 0.  */
  value_bit () : m_index (0)
  {};

  /* Constructor that sets m_index to the specified value.  */
  value_bit (size_t i) : m_index (i)
  {};

  /* Copy constructor for value_bit.  */
  value_bit (const value_bit &val) : m_index (val.m_index)
  {};

  /* Returns the bit's initial index in bit-vector.  */
  size_t get_index () const;

  /* Returns type of the bit.  */
  value_type get_type () const;

  /* This will support deep copy of objects' values.  */
  virtual value_bit *copy () const = 0;

  /* Prints the bit.  Inherited classes must implement it.  */
  virtual void print () = 0;

  /* Destructor.  */
  virtual ~value_bit () = default;
};


/* Represents value of a single bit of symbolic marked variables.  */

class symbolic_bit : public value_bit {
  /* The Origin of the bit.  */
  tree m_origin = nullptr;

 public:
  /* Constructor that sets the bit's initial position and its origin.  */
  symbolic_bit (size_t i, tree orig);

  /* Copy constructor for symbolic_bit.  */
  symbolic_bit (const symbolic_bit &sym_bit) : symbolic_bit (sym_bit.m_index,
							     sym_bit.m_origin)
  {};

  /* Returns a copy of the bit.  */
  value_bit *copy () const;

  /* Prints the bit.  */
  void print ();

  /* Returns the origin of the bit, to whom it belongs.  */
  tree get_origin ();
};


/* Represents value of a single bit.  */

class bit : public value_bit {
 private:
  /* This is the value of a bit.  It must be either 1 or 0.  */
  unsigned char m_val = 0;

 public:
  /* Constructor that sets m_val to the specified value.  */
  bit (unsigned char i);

  /* Copy constructor for bit.  */
  bit (const bit &b) : bit (b.m_val)
  {};

  /* Returns the value of the bit.  */
  unsigned char get_val () const;

  /* Sets the value of the bit.  */
  void set_val (unsigned char new_val);

  /* Return a copy of the bit.  */
  value_bit *copy () const;

  /* Prints the bit.  */
  void print ();
};


/* Bit-level base expression class.  In general expressions consist of
   two operands.  Here we named them m_left and m_right.  */

class bit_expression : public value_bit {
 protected:
  /* The bit left to the expression sign.  */
  value_bit *m_left = nullptr;

  /* The bit right to the expression sign.  */
  value_bit *m_right = nullptr;

  /* Copies the given expression to it by copying
     the left and right operands.  */
  void copy (const bit_expression *expr);

  /* Depending on the expression, prints its sign.  */
  virtual void print_expr_sign ();

 public:

  /* Returns left operand of the expression.  */
  value_bit *get_left ();

  /* Returns right operand of the expression.  */
  value_bit *get_right ();

  /* Destructor for bit_expression.  */
  ~bit_expression ();

  /* Sets left operand of the expression.  */
  void set_left (value_bit *expr);

  /* Sets right operand of the expression.  */
  void set_right (value_bit *expr);

  /* Returns a deep copy of the expression.  */
  value_bit *copy () const = 0;

  /* Prints the expression.  */
  void print ();
};


/* Bit-level XOR expression.  XOR operation on two variables (when one of
   them is symbolic) can be represented by XOR operations on
   each of their bits.  */

class bit_xor_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the bit_xor_expression sign.  */
  bit_xor_expression (value_bit *left, value_bit *right);

  /* Copy constructor for bit_xor_expression.  */
  bit_xor_expression (const bit_xor_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* Bit-level AND expression.  AND operation on two variables (when one of
   them is symbolic) can be represented by AND operations on
   each of their bits.  */

class bit_and_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the bit_and_expression sign.  */
  bit_and_expression (value_bit *left, value_bit *right);

  /* Copy constructor for bit_and_expression.  */
  bit_and_expression (const bit_and_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* Bit-level OR expression.  OR operation on two variables (when one of
   them is symbolic) can be represented by OR operations on
   each of their bits.  */

class bit_or_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the bit_or_expression sign.  */
  bit_or_expression (value_bit *left, value_bit *right);

  /* Copy constructor for bit_or_expression.  */
  bit_or_expression (const bit_or_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* SHIFT_RIGHT expression.  Result must be stored bit by bit.  */

class shift_right_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the shift_right_expression sign.  */
  shift_right_expression (value_bit *left, value_bit *right);

  /* Copy constructor for shift_right_expression.  */
  shift_right_expression (const shift_right_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* SHIFT_LEFT expression.  Result must be stored bit by bit.  */

class shift_left_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the shift_left_expression sign.  */
  shift_left_expression (value_bit *left, value_bit *right);

  /* Copy constructor for shift_left_expression.  */
  shift_left_expression (const shift_left_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* ADD expression.  Result must be stored bit by bit.  */

class add_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the add_expression sign.  */
  add_expression (value_bit *left, value_bit *right);

  /* Copy constructor for add_expression.  */
  add_expression (const add_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* SUB expression.  Result must be stored bit by bit.  */

class sub_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the sub_expression sign.  */
  sub_expression (value_bit *left, value_bit *right);

  /* Copy constructor for sub_expression.  */
  sub_expression (const sub_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;
};


/* Bit-level negation expression.  */

class bit_complement_expression : public bit_expression {
 public:
  /* Constructor that sets the left and right side bits
     of the bit_complement_expression sign.  */
  bit_complement_expression (value_bit *right);

  /* Copy constructor for bit_complement_expression.  */
  bit_complement_expression (const bit_complement_expression &expr);

  /* Returns a copy of the expression.  */
  value_bit *copy () const;

  /* Prints the expression.  */
  void print ();
};

#endif /* SYM_EXEC_EXPRESSION_H.  */
