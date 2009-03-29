/* Test that a condition is not counted as a call to
   __builtin_constant_p if that call is itself inside a conditional
   expression with __builtin_constant_p condition.  */
/* { dg-do compile } */

extern int foo(void);

#define X(exp) (__builtin_choose_expr(1, __builtin_constant_p(exp), 1) ? (exp) : -1)

const int x = ((__builtin_constant_p(1) ? __builtin_constant_p (0 && foo()) : 0) ? (0 && foo()) : -1); /* { dg-error "initializer element is not a constant expression" } */
