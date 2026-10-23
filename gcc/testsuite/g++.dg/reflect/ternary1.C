// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test is_conditional_operator, condition_of, true_expression_of and
// false_expression_of.

#include <meta>

using namespace std::meta;

constexpr float relu(float x) {
  return x > 0.0f ? x : 0.0f;
}

constexpr float abs_val(float x) {
  return x > 0.0f ? x : -x;
}

constexpr float clamp01(float x) {
  return x < 0.0f ? 0.0f : (x > 1.0f ? 1.0f : x);
}

constexpr int elvis(int x) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
  return x ?: 42;
#pragma GCC diagnostic pop
}

constexpr float abs_if(float x) {
  if (x > 0.0f)
    return x;
  return -x;
}

constexpr float plain(float x) {
  return x + 1.0f;
}

consteval info ret_of(info fn) {
  return return_operand_of(statements_of(body_of(fn)).back());
}

// relu: x > 0 ? x : 0
consteval bool test_relu() {
  info e = ret_of(^^relu);
  if (!is_expression(e)) return false;
  if (!is_conditional_operator(e)) return false;

  info c = condition_of(e);
  if (!is_binary_operator(c)) return false;
  if (operator_of(c) != operators::op_greater) return false;

  if (!is_variable(true_expression_of(e))) return false;
  if (!is_literal(false_expression_of(e))) return false;

  // operands_of walks the same three operands in written order.
  auto ops = operands_of(e);
  if (ops.size() != 3) return false;
  if (ops[0] != condition_of(e)) return false;
  if (ops[1] != true_expression_of(e)) return false;
  if (ops[2] != false_expression_of(e)) return false;
  return true;
}

// abs_val: x > 0 ? x : -x
consteval bool test_abs() {
  info e = ret_of(^^abs_val);
  if (!is_conditional_operator(e)) return false;

  info f = false_expression_of(e);
  if (!is_unary_operator(f)) return false;
  if (operator_of(f) != operators::op_minus) return false;
  return true;
}

// clamp01: x < 0 ? 0 : (x > 1 ? 1 : x)
consteval bool test_nested() {
  info e = ret_of(^^clamp01);
  if (!is_conditional_operator(e)) return false;
  if (operator_of(condition_of(e)) != operators::op_less) return false;
  if (!is_literal(true_expression_of(e))) return false;

  info inner = false_expression_of(e);
  if (!is_conditional_operator(inner)) return false;
  if (operator_of(condition_of(inner)) != operators::op_greater) return false;
  if (!is_literal(true_expression_of(inner))) return false;
  if (!is_variable(false_expression_of(inner))) return false;
  return true;
}

// elvis: x ?: 42 (GNU extension)
consteval bool test_elvis() {
  info e = ret_of(^^elvis);
  if (!is_conditional_operator(e)) return false;
  // The SAVE_EXPR placeholder is stripped, so the true branch is x itself.
  if (!is_variable(true_expression_of(e))) return false;
  if (!is_literal(false_expression_of(e))) return false;
  return true;
}

// abs_if: an if statement is a void COND_EXPR -- not a conditional operator,
// but its condition and branches can still be navigated.
consteval bool test_if_statement() {
  info s = statements_of(body_of(^^abs_if))[0];
  if (is_conditional_operator(s)) return false;

  info c = condition_of(s);
  if (!is_binary_operator(c)) return false;
  if (operator_of(c) != operators::op_greater) return false;

  if (!is_return_statement(true_expression_of(s))) return false;
  return true;
}

// Non-conditionals.
consteval bool test_negative() {
  if (is_conditional_operator(ret_of(^^plain))) return false;
  if (is_conditional_operator(^^int)) return false;
  if (is_conditional_operator(^^relu)) return false;

  try {
    condition_of(ret_of(^^plain));
    return false;
  } catch (const std::meta::exception &) {
  }
  return true;
}

static_assert(test_relu());
static_assert(test_abs());
static_assert(test_nested());
static_assert(test_elvis());
static_assert(test_if_statement());
static_assert(test_negative());
