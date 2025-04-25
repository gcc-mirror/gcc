/* Reject expressions outside of a numeric range.  */

void v_0(int, int, int) {}
void v_1(int, int, int) {}
void v_2(int, int, int) {}
void v_3(int, int, int) {}

enum {constant_expression = 1};

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


/* Invalid uses of omp_num_args.  */

void ona_v0 (int, int, int) {}
void ona_v1 (int, int, int) {}
void ona_v2 (int, int, int) {}
void ona_v3 (int, int, int) {}

/* { dg-error "'omp_num_args' may only be used at the start of a numeric range bound" "" { target *-*-* } .+2 } */
#pragma omp declare variant(ona_v0) match(construct={dispatch}) \
				    adjust_args(nothing: omp_num_args)
void ona_b0 (int, int, int) {}

/* { dg-error "'omp_num_args' may only be used at the start of a numeric range bound" "" { target *-*-* } .+3 } */
/* { dg-error "'omp_num_args' may only be used at the start of a numeric range bound" "" { target *-*-* } .+3 } */
#pragma omp declare variant(ona_v1) match(construct={dispatch}) \
				    adjust_args(nothing: omp_num_args, \
				    			 omp_num_args)
void ona_b1 (int, int, int) {}

/* { dg-error "'omp_num_args' may only be used at the start of a numeric range bound" "" { target *-*-* } .+2 } */
#pragma omp declare variant(ona_v2) match(construct={dispatch}) \
				    adjust_args(nothing: omp_num_args, 1)
void ona_b2 (int, int, int) {}

/* { dg-error "'omp_num_args' may only be used at the start of a numeric range bound" "" { target *-*-* } .+2 } */
#pragma omp declare variant(ona_v3) match(construct={dispatch}) \
				    adjust_args(nothing: 1, omp_num_args)
void ona_b3 (int, int, int) {}
