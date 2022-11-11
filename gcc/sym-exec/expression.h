/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.  */

#ifndef SYM_EXEC_EXPRESSION_H
#define SYM_EXEC_EXPRESSION_H

#include "stddef.h"

/* Base class for single bit value.  */

class value {
 protected:
  /* This will help us to understand where is moved the bit
     from its initial position.  */
  const size_t index;

 public:
  value () : index (0)
  {};
  value (size_t i) : index (i)
  {};
  value (value &val) : index (val.index)
  {};
  size_t get_index () const;

  /* This will support deep copy of objects' values.  */
  virtual value *copy () const = 0;
  virtual ~value ()
  {};
};


/* Represents value of a single bit of symbolic marked variables.  */

class symbolic_bit : public value {
 public:
  symbolic_bit (size_t i) : value (i)
  {};
  symbolic_bit (const symbolic_bit &sym_bit) : symbolic_bit (sym_bit.index)
  {};

  value *copy () const;
};


/* Represents value of a single bit.  */

class bit : public value {
 private:
  /* This is the value of a bit.  It must be either 1 or 0.  */
  unsigned char val = 0;

 public:
  bit (unsigned char i) : val (i)
  {};
  bit (const bit &b) : bit (b.val)
  {};
  unsigned char get_val () const;
  void set_val (unsigned char new_val);
  value *copy () const;
};


/* Bit-level base expression class.  In general expressions consist of
   two operands.  Here we named them left and right.  */

class bit_expression : public value {
 protected:
  value *left = nullptr;
  value *right = nullptr;

 public:
  bit_expression () : left (nullptr), right (nullptr)
  {};
  bit_expression (value *left, value *right);

  bit_expression (const bit_expression &expr);

  value *get_left ();
  value *get_right ();

  void set_left (value *expr);
  void set_right (value *expr);
  value *copy () const;
  virtual ~bit_expression ();
};


/* Bit-level XOR expression.  XOR operation on two variables (when one of
   them is symbolic) can be represented by XOR operations on
   each of their bits.  */

class bit_xor_expression : public bit_expression {
 public:
  bit_xor_expression (value *left, value *right) : bit_expression (left, right)
  {};
  bit_xor_expression (const bit_xor_expression &expr) : bit_expression (expr)
  {};
  value *copy () const;
};


/* Bit-level AND expression.  AND operation on two variables (when one of
   them is symbolic) can be represented by AND operations on
   each of their bits.  */

class bit_and_expression : public bit_expression {
 public:
  bit_and_expression (value *left, value *right) : bit_expression (left, right)
  {};
  bit_and_expression (const bit_and_expression &expr) : bit_expression (expr)
  {};
  value *copy () const;
};


/* Bit-level OR expression.  OR operation on two variables (when one of
   them is symbolic) can be represented by OR operations on
   each of their bits.  */

class bit_or_expression : public bit_expression {
 public:
  bit_or_expression (value *left, value *right) : bit_expression (left, right)
  {};
  bit_or_expression (bit_or_expression &expr) : bit_expression (expr)
  {};
  value *copy () const;
};


/* SHIFT_RIGHT expression.  Result must be stored bit by bit.  */

class shift_right_expression : public bit_expression {
 public:
  shift_right_expression (value *left, value *right) : bit_expression (left,
								       right)
  {};
  shift_right_expression (const shift_right_expression &expr)
			 : bit_expression (expr)
  {};
  value *copy () const;
};


/* SHIFT_LEFT expression.  Result must be stored bit by bit.  */

class shift_left_expression : public bit_expression {
 public:
  shift_left_expression (value *left, value *right) : bit_expression (left,
								      right)
  {};
  shift_left_expression (const shift_left_expression &expr)
			: bit_expression (expr)
  {};
  value *copy () const;
};


/* ADD expression.  Result must be stored bit by bit.  */

class add_expression : public bit_expression {
 public:
  add_expression (value *left, value *right) : bit_expression (left, right)
  {};
  add_expression (const add_expression &expr) : bit_expression (expr)
  {};
  value *copy () const;
};


/* SUB expression.  Result must be stored bit by bit.  */

class sub_expression : public bit_expression {
 public:
  sub_expression (value *left, value *right) : bit_expression (left, right)
  {};
  sub_expression (const sub_expression &expr) : bit_expression (expr)
  {};
  value *copy () const;
};

/* Bit-level negation expression.  */

class bit_complement_expression : public bit_expression {
 public:
  bit_complement_expression (value *right);
  bit_complement_expression (const bit_complement_expression &expr)
  			    : bit_expression (expr)
  {};
  value *copy () const;
};

#endif /* SYM_EXEC_EXPRESSION_H.  */
