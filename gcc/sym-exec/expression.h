/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.  */

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
  value_type m_type;

 public:
  value_bit () : m_index (0)
  {};
  value_bit (size_t i) : m_index (i)
  {};
  value_bit (const value_bit &val) : m_index (val.m_index)
  {};
  size_t get_index () const;
  value_type get_type () const;

  /* This will support deep copy of objects' values.  */
  virtual value_bit *copy () const = 0;
  virtual void print () = 0;
  virtual ~value_bit () = default;
};

/* Represents value of a single bit of symbolic marked variables.  */

class symbolic_bit : public value_bit {
  tree m_origin = nullptr;

 public:
  symbolic_bit (size_t i, tree orig);
  symbolic_bit (const symbolic_bit &sym_bit) : symbolic_bit (sym_bit.m_index,
							     sym_bit.m_origin)
  {};

  value_bit *copy () const;
  void print ();
  tree get_origin ();
};


/* Represents value of a single bit.  */

class bit : public value_bit {
 private:
  /* This is the value of a bit.  It must be either 1 or 0.  */
  unsigned char m_val = 0;

 public:
  bit (unsigned char i);
  bit (const bit &b) : bit (b.m_val)
  {};
  unsigned char get_val () const;
  void set_val (unsigned char new_val);
  value_bit *copy () const;
  void print ();
};


/* Bit-level base expression class.  In general expressions consist of
   two operands.  Here we named them m_left and right.  */

class bit_expression : public value_bit {
 protected:
  value_bit *m_left = nullptr;
  value_bit *m_right = nullptr;

  void copy (const bit_expression *expr);
  virtual void print_expr_sign ();

 public:
  value_bit *get_left ();
  value_bit *get_right ();

  ~bit_expression ();

  void set_left (value_bit *expr);
  void set_right (value_bit *expr);
  value_bit *copy () const = 0;
  void print ();
};


/* Bit-level XOR expression.  XOR operation on two variables (when one of
   them is symbolic) can be represented by XOR operations on
   each of their bits.  */

class bit_xor_expression : public bit_expression {
 public:
  bit_xor_expression (value_bit *left, value_bit *right);
  bit_xor_expression (const bit_xor_expression &expr);
  value_bit *copy () const;
};


/* Bit-level AND expression.  AND operation on two variables (when one of
   them is symbolic) can be represented by AND operations on
   each of their bits.  */

class bit_and_expression : public bit_expression {
 public:
  bit_and_expression (value_bit *left, value_bit *right);
  bit_and_expression (const bit_and_expression &expr);
  value_bit *copy () const;
};


/* Bit-level OR expression.  OR operation on two variables (when one of
   them is symbolic) can be represented by OR operations on
   each of their bits.  */

class bit_or_expression : public bit_expression {
 public:
  bit_or_expression (value_bit *left, value_bit *right);
  bit_or_expression (const bit_or_expression &expr);
  value_bit *copy () const;
};


/* SHIFT_RIGHT expression.  Result must be stored bit by bit.  */

class shift_right_expression : public bit_expression {
 public:
  shift_right_expression (value_bit *left, value_bit *right);
  shift_right_expression (const shift_right_expression &expr);
  value_bit *copy () const;
};


/* SHIFT_LEFT expression.  Result must be stored bit by bit.  */

class shift_left_expression : public bit_expression {
 public:
  shift_left_expression (value_bit *left, value_bit *right);
  shift_left_expression (const shift_left_expression &expr);
  value_bit *copy () const;
};


/* ADD expression.  Result must be stored bit by bit.  */

class add_expression : public bit_expression {
 public:
  add_expression (value_bit *left, value_bit *right);
  add_expression (const add_expression &expr);
  value_bit *copy () const;
};


/* SUB expression.  Result must be stored bit by bit.  */

class sub_expression : public bit_expression {
 public:
  sub_expression (value_bit *left, value_bit *right);
  sub_expression (const sub_expression &expr);
  value_bit *copy () const;
};

/* Bit-level negation expression.  */

class bit_complement_expression : public bit_expression {
 public:
  bit_complement_expression (value_bit *right);
  bit_complement_expression (const bit_complement_expression &expr);
  value_bit *copy () const;
  void print ();
};

#endif /* SYM_EXEC_EXPRESSION_H.  */