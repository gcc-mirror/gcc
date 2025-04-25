/* Reject expressions outside of a numeric range.  */

void v_0(int, int, int) {}
void v_1(int, int, int) {}
void v_2(int, int, int) {}
void v_3(int, int, int) {}

const int constant_expression = 42;

/* { dg-error "expected ':' before '\\)' token" "" { target *-*-* } .+2 } */
/* { dg-note "an expression is only allowed in a numeric range" "" { target *-*-* } .+1 } */
#pragma omp declare variant (v_0) match (construct={dispatch}) adjust_args (nothing: 0+1)
void b0 (int, int, int) {}

/* { dg-error "'constant_expression' is not a function parameter" "" { target *-*-* } .+2 } */
/* { dg-note "an expression is only allowed in a numeric range" "" { xfail *-*-* } .+1 } */
#pragma omp declare variant (v_1) match (construct={dispatch}) adjust_args (nothing: constant_expression)
void b1 (int, int, int) {}

/* { dg-error "expected ':' before '\\)' token" "" { target *-*-* } .+2 } */
/* { dg-note "an expression is only allowed in a numeric range" "" { target *-*-* } .+1 } */
#pragma omp declare variant (v_2) match (construct={dispatch}) adjust_args (nothing: constant_expression + 0)
void b2 (int, int, int) {}

/* { dg-error "expected ':' before '\\)' token" "" { target *-*-* } .+2 } */
/* { dg-note "an expression is only allowed in a numeric range" "" { target *-*-* } .+1 } */
#pragma omp declare variant (v_3) match (construct={dispatch}) adjust_args (nothing: 0 + constant_expression)
void b3 (int, int, int) {}

/* We could use some tests to make sure non constant-expressions get diagnosed nicely.  */
