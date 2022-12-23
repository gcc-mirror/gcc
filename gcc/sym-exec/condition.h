#ifndef SYM_EXEC_CONDITION_H
#define SYM_EXEC_CONDITION_H

#include "expression.h"

enum condition_type
{
  GREAT_THAN,
  LESS_THAN,
  EQUAL,
  NOT_EQUAL,
  IS_FALSE,
  IS_TRUE
};


enum condition_status
{
  CS_NO_COND,
  CS_TRUE,
  CS_FALSE,
  CS_SYM
};


class bit_condition : public bit_expression
{
 private:
  condition_type type;

 public:
  bit_condition (value_bit* left, value_bit* right, condition_type type);
  bit_condition (const bit_condition &expr);
  condition_type get_cond_type () const;
  value_bit *copy () const;
  value_type get_type () const;
};




#endif /* SYM_EXEC_CONDITION_H.  */