#ifndef SYM_EXEC_CONDITION_H
#define SYM_EXEC_CONDITION_H

#include "expression.h"

enum condition_type
{
  GREAT_THAN,
  LESS_THAN,
  NOT_ZERO,
  EQUAL,
  NOT_EQUAL,
  GREAT_OR_EQUAL,
  IS_FALSE,
  IS_TRUE
};


class bit_condition : public bit_expression
{
 private:
  condition_type type;

 public:
  bit_condition (value* left, value* right, condition_type type);
  bit_condition (const bit_condition &expr);
  condition_type get_cond_type () const;
  value *copy () const;
  value_type get_type () const;
};




#endif /* SYM_EXEC_CONDITION_H.  */