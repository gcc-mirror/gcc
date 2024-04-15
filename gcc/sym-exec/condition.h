#ifndef SYM_EXEC_CONDITION_H
#define SYM_EXEC_CONDITION_H

#include "expression.h"

enum condition_status {
  CS_NO_COND,
  CS_TRUE,
  CS_FALSE,
  CS_SYM
};


class bit_condition : public bit_expression {
 private:
  tree_code m_code;
  void print_expr_sign ();

 public:
  bit_condition (value_bit *left, value_bit *right, tree_code type);
  bit_condition (const bit_condition &expr);
  tree_code get_code () const;
  value_bit *copy () const;
};

#endif /* SYM_EXEC_CONDITION_H.  */