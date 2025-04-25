/* Test uses of omp_num_args.  */

void v_1_arg_0(int) {}
void v_1_arg_1(int) {}
void v_1_arg_2(int) {}
void v_1_arg_3(int) {}
void v_1_arg_4(int) {}
void v_1_arg_5(int) {}
void v_1_arg_6(int) {}
void v_1_arg_7(int) {}
void v_1_arg_8(int) {}
void v_1_arg_9(int) {}
void v_1_arg_10(int) {}
void v_1_arg_11(int) {}
void v_1_arg_12(int) {}
void v_1_arg_13(int) {}
void v_1_arg_14(int) {}
void v_1_arg_15(int) {}
void v_1_arg_16(int) {}
void v_1_arg_17(int) {}
void v_1_arg_18(int) {}
void v_1_arg_19(int) {}
void v_1_arg_20(int) {}
void v_1_arg_21(int) {}
void v_1_arg_22(int) {}
void v_1_arg_23(int) {}
void v_1_arg_24(int) {}


// literal

#pragma omp declare variant (v_1_arg_0) match (construct={dispatch}) adjust_args (nothing: 1:1)
void b_1_arg_literal_literal_0 (int) {}

// defaults (lb default is 1, ub default is omp_num_args)

#pragma omp declare variant (v_1_arg_1) match (construct={dispatch}) adjust_args (nothing: :)
void b_1_arg_default_default (int) {}

#pragma omp declare variant (v_1_arg_2) match (construct={dispatch}) adjust_args (nothing: :1)
void b_1_arg_default_literal_0 (int) {}

#pragma omp declare variant (v_1_arg_3) match (construct={dispatch}) adjust_args (nothing: :omp_num_args)
void b_1_arg_default_numargs_0 (int) {}

#pragma omp declare variant (v_1_arg_4) match (construct={dispatch}) adjust_args (nothing: :omp_num_args+0)
void b_1_arg_default_numargs_1 (int) {}

#pragma omp declare variant (v_1_arg_5) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-0)
void b_1_arg_default_numargs_2 (int) {}

#pragma omp declare variant (v_1_arg_6) match (construct={dispatch}) adjust_args (nothing: 1:)
void b_1_arg_literal_default_0 (int) {}

#pragma omp declare variant (v_1_arg_7) match (construct={dispatch}) adjust_args (nothing: omp_num_args:)
void b_1_arg_numargs_default_0 (int) {}

#pragma omp declare variant (v_1_arg_8) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:)
void b_1_arg_numargs_default_1 (int) {}

#pragma omp declare variant (v_1_arg_9) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:)
void b_1_arg_numargs_default_2 (int) {}


// literal : omp_num_args+/-

#pragma omp declare variant (v_1_arg_10) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args)
void b_1_arg_literal_numargs_0 (int) {}

#pragma omp declare variant (v_1_arg_11) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args+0)
void b_1_arg_literal_numargs_1 (int) {}

#pragma omp declare variant (v_1_arg_12) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-0)
void b_1_arg_literal_numargs_2 (int) {}

// omp_num_args+/- : literal

#pragma omp declare variant (v_1_arg_13) match (construct={dispatch}) adjust_args (nothing: omp_num_args:1)
void b_1_arg_numargs_literal_0 (int) {}

#pragma omp declare variant (v_1_arg_14) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:1)
void b_1_arg_numargs_literal_1 (int) {}

#pragma omp declare variant (v_1_arg_15) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:1)
void b_1_arg_numargs_literal_2 (int) {}

// omp_num_args+/- : omp_num_args+/-
// we need to avoid combinatorial explosion here...

#pragma omp declare variant (v_1_arg_16) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args)
void b_1_arg_numargs_numargs_0_0 (int) {}

#pragma omp declare variant (v_1_arg_17) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args+0)
void b_1_arg_numargs_numargs_0_1 (int) {}

#pragma omp declare variant (v_1_arg_18) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-0)
void b_1_arg_numargs_numargs_0_2 (int) {}

#pragma omp declare variant (v_1_arg_19) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args)
void b_1_arg_numargs_numargs_1_0 (int) {}

#pragma omp declare variant (v_1_arg_20) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args+0)
void b_1_arg_numargs_numargs_1_1 (int) {}

#pragma omp declare variant (v_1_arg_21) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-0)
void b_1_arg_numargs_numargs_1_2 (int) {}

#pragma omp declare variant (v_1_arg_22) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args)
void b_1_arg_numargs_numargs_2_0 (int) {}

#pragma omp declare variant (v_1_arg_23) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args+0)
void b_1_arg_numargs_numargs_2_1 (int) {}

#pragma omp declare variant (v_1_arg_24) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-0)
void b_1_arg_numargs_numargs_2_2 (int) {}



void v_2_arg(int, int) {}

// literal

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:1)
void b_2_arg_literal_literal_0_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:2)
void b_2_arg_literal_literal_0_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:2)
void b_2_arg_literal_literal_1_1 (int, int) {}

// defaults (lb default is 1, ub default is omp_num_args)

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :)
void b_2_arg_default_default (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :1)
void b_2_arg_default_literal_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :2)
void b_2_arg_default_literal_2 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args)
void b_2_arg_default_numargs_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args+0)
void b_2_arg_default_numargs_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-0)
void b_2_arg_default_numargs_2 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-1)
void b_2_arg_default_numargs_3 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:)
void b_2_arg_literal_default_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:)
void b_2_arg_literal_default_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:)
void b_2_arg_numargs_default_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:)
void b_2_arg_numargs_default_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:)
void b_2_arg_numargs_default_2 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:)
void b_2_arg_numargs_default_3 (int, int) {}

// literal : omp_num_args+/-

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args)
void b_2_arg_literal_numargs_0_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args+0)
void b_2_arg_literal_numargs_0_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-0)
void b_2_arg_literal_numargs_0_2 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1)
void b_2_arg_literal_numargs_0_3 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args)
void b_2_arg_literal_numargs_1_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args+0)
void b_2_arg_literal_numargs_1_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args-0)
void b_2_arg_literal_numargs_1_2 (int, int) {}

/* Out of range
#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args-1)
void b_2_arg_literal_numargs_1_3 (int, int) {}  */


// omp_num_args+/- : literal

/* Out of range
#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:1)
void b_2_arg_numargs_literal_0_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:1)
void b_2_arg_numargs_literal_1_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:1)
void b_2_arg_numargs_literal_2_0 (int, int) {}  */

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:1)
void b_2_arg_numargs_literal_3_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:2)
void b_2_arg_numargs_literal_0_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:2)
void b_2_arg_numargs_literal_1_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:2)
void b_2_arg_numargs_literal_2_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:2)
void b_2_arg_numargs_literal_3_1 (int, int) {}

// omp_num_args+/- : omp_num_args+/-

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args)
void b_2_arg_numargs_numargs_0_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args+0)
void b_2_arg_numargs_numargs_0_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-0)
void b_2_arg_numargs_numargs_0_2 (int, int) {}
/* Out of range
#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-1)
void b_2_arg_numargs_numargs_0_3 (int, int) {}  */

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args)
void b_2_arg_numargs_numargs_1_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args+0)
void b_2_arg_numargs_numargs_1_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-0)
void b_2_arg_numargs_numargs_1_2 (int, int) {}
/* Out of range
#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-1)
void b_2_arg_numargs_numargs_1_3 (int, int) {}  */

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args)
void b_2_arg_numargs_numargs_2_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args+0)
void b_2_arg_numargs_numargs_2_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-0)
void b_2_arg_numargs_numargs_2_2 (int, int) {}
/* Out of range
#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-1)
void b_2_arg_numargs_numargs_2_3 (int, int) {}  */

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args)
void b_2_arg_numargs_numargs_3_0 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args+0)
void b_2_arg_numargs_numargs_3_1 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args-0)
void b_2_arg_numargs_numargs_3_2 (int, int) {}

#pragma omp declare variant (v_2_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args-1)
void b_2_arg_numargs_numargs_3_3 (int, int) {}



void v_3_arg(int, int, int) {}

// literal

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:1)
void b_3_arg_literal_literal_0_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:2)
void b_3_arg_literal_literal_0_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:3)
void b_3_arg_literal_literal_0_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:2)
void b_3_arg_literal_literal_1_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:3)
void b_3_arg_literal_literal_1_2 (int, int, int) {}

// defaults (lb default is 1, ub default is omp_num_args)

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :)
void b_3_arg_default_default (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :1)
void b_3_arg_default_literal_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :2)
void b_3_arg_default_literal_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :3)
void b_3_arg_default_literal_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args)
void b_3_arg_default_numargs_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args+0)
void b_3_arg_default_numargs_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-0)
void b_3_arg_default_numargs_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-1)
void b_3_arg_default_numargs_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: :omp_num_args-2)
void b_3_arg_default_numargs_4 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:)
void b_3_arg_literal_default_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:)
void b_3_arg_literal_default_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:)
void b_3_arg_literal_default_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:)
void b_3_arg_numargs_default_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:)
void b_3_arg_numargs_default_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:)
void b_3_arg_numargs_default_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:)
void b_3_arg_numargs_default_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:)
void b_3_arg_numargs_default_4 (int, int, int) {}

// literal : omp_num_args+/-

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args)
void b_3_arg_literal_numargs_0_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args+0)
void b_3_arg_literal_numargs_0_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-0)
void b_3_arg_literal_numargs_0_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1)
void b_3_arg_literal_numargs_0_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-2)
void b_3_arg_literal_numargs_0_4 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args)
void b_3_arg_literal_numargs_1_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args+0)
void b_3_arg_literal_numargs_1_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args-0)
void b_3_arg_literal_numargs_1_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args-1)
void b_3_arg_literal_numargs_1_3 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args-2)
void b_3_arg_literal_numargs_1_4 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:omp_num_args)
void b_3_arg_literal_numargs_2_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:omp_num_args+0)
void b_3_arg_literal_numargs_2_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:omp_num_args-0)
void b_3_arg_literal_numargs_2_2 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:omp_num_args-1)
void b_3_arg_literal_numargs_2_3 (int, int, int) {}
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: 3:omp_num_args-2)
void b_3_arg_literal_numargs_2_4 (int, int, int) {}  */


// omp_num_args+/- : literal

/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:1)
void b_3_arg_numargs_literal_0_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:1)
void b_3_arg_numargs_literal_1_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:1)
void b_3_arg_numargs_literal_2_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:1)
void b_3_arg_numargs_literal_3_0 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:1)
void b_3_arg_numargs_literal_4_0 (int, int, int) {}

/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:2)
void b_3_arg_numargs_literal_0_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:2)
void b_3_arg_numargs_literal_1_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:2)
void b_3_arg_numargs_literal_2_1 (int, int, int) {}  */


#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:2)
void b_3_arg_numargs_literal_3_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:2)
void b_3_arg_numargs_literal_4_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:3)
void b_3_arg_numargs_literal_0_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:3)
void b_3_arg_numargs_literal_1_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:3)
void b_3_arg_numargs_literal_2_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:3)
void b_3_arg_numargs_literal_3_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:3)
void b_3_arg_numargs_literal_4_2 (int, int, int) {}

// omp_num_args+/- : omp_num_args+/-

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args)
void b_3_arg_numargs_numargs_0_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args+0)
void b_3_arg_numargs_numargs_0_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-0)
void b_3_arg_numargs_numargs_0_2 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-1)
void b_3_arg_numargs_numargs_0_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args:omp_num_args-2)
void b_3_arg_numargs_numargs_0_4 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args)
void b_3_arg_numargs_numargs_1_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args+0)
void b_3_arg_numargs_numargs_1_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-0)
void b_3_arg_numargs_numargs_1_2 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-1)
void b_3_arg_numargs_numargs_1_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args+0:omp_num_args-2)
void b_3_arg_numargs_numargs_1_4 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args)
void b_3_arg_numargs_numargs_2_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args+0)
void b_3_arg_numargs_numargs_2_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-0)
void b_3_arg_numargs_numargs_2_2 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-1)
void b_3_arg_numargs_numargs_2_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-0:omp_num_args-2)
void b_3_arg_numargs_numargs_2_4 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args)
void b_3_arg_numargs_numargs_3_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args+0)
void b_3_arg_numargs_numargs_3_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args-0)
void b_3_arg_numargs_numargs_3_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args-1)
void b_3_arg_numargs_numargs_3_3 (int, int, int) {}
/* Out of range
#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-1:omp_num_args-2)
void b_3_arg_numargs_numargs_3_4 (int, int, int) {}  */

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:omp_num_args)
void b_3_arg_numargs_numargs_4_0 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:omp_num_args+0)
void b_3_arg_numargs_numargs_4_1 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:omp_num_args-0)
void b_3_arg_numargs_numargs_4_2 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:omp_num_args-1)
void b_3_arg_numargs_numargs_4_3 (int, int, int) {}

#pragma omp declare variant (v_3_arg) match (construct={dispatch}) adjust_args (nothing: omp_num_args-2:omp_num_args-2)
void b_3_arg_numargs_numargs_4_4 (int, int, int) {}


/* 1-3 args should be fine for now.
void v_4_arg(int, int, int, int) {}
void v_5_arg(int, int, int, int, int) {}  */
