/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Test uses of omp_num_args in a variadic function.  */

/* NOTE: Make sure the arguments passed to the functions have unique names to
   not interfere with the dg-final checks.  */

void v0_0_arg_vari_0(...) {}
void v0_0_arg_vari_1(...) {}
void v0_0_arg_vari_2(...) {}
void v0_0_arg_vari_3(...) {}
void v0_0_arg_vari_4(...) {}
void v0_0_arg_vari_5(...) {}
void v0_0_arg_vari_6(...) {}
void v0_0_arg_vari_7(...) {}

/* All args adjusted.  */

// defaults

#pragma omp declare variant (v0_0_arg_vari_0) match (construct={dispatch}) adjust_args (need_device_ptr: :)
void b_default_default (...) {}

#pragma omp declare variant (v0_0_arg_vari_1) match (construct={dispatch}) adjust_args (need_device_ptr: :omp_num_args)
void b_default_numargs_0 (...) {}

#pragma omp declare variant (v0_0_arg_vari_2) match (construct={dispatch}) adjust_args (need_device_ptr: :omp_num_args+0)
void b_default_numargs_1 (...) {}

#pragma omp declare variant (v0_0_arg_vari_3) match (construct={dispatch}) adjust_args (need_device_ptr: :omp_num_args-0)
void b_default_numargs_2 (...) {}

#pragma omp declare variant (v0_0_arg_vari_4) match (construct={dispatch}) adjust_args (need_device_ptr: 1:)
void b_literal_default_0 (...) {}

// literal : omp_num_args+/-

#pragma omp declare variant (v0_0_arg_vari_5) match (construct={dispatch}) adjust_args (need_device_ptr: 1:omp_num_args)
void b_literal_numargs_0 (...) {}

#pragma omp declare variant (v0_0_arg_vari_6) match (construct={dispatch}) adjust_args (need_device_ptr: 1:omp_num_args+0)
void b_literal_numargs_1 (...) {}

#pragma omp declare variant (v0_0_arg_vari_7) match (construct={dispatch}) adjust_args (need_device_ptr: 1:omp_num_args-0)
void b_literal_numargs_2 (...) {}

/* 8 function calls.  */
#define PASS_ARGS_TO_ALL(...) \
  do {						\
    _Pragma("omp dispatch")			\
    b_default_default(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_default_numargs_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_default_numargs_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_default_numargs_2(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_literal_default_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_literal_numargs_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_literal_numargs_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")			\
    b_literal_numargs_2(__VA_ARGS__);		\
  } while (0);

void do_all_args(int *p0)
{
  /* 6 uses of p0, times 8, 6*8=48.  */
  /* 3 expansions, times 8, 3*8=24 uses of omp dispatch.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" 48 "gimple" } }  */
  PASS_ARGS_TO_ALL (p0);
/* { dg-final { scan-tree-dump-times "v0_0_arg_vari_\[0-7\] \\(D\.\[0-9\]+\\);" 8 "gimple" } }  */
  PASS_ARGS_TO_ALL (p0, p0);
/* { dg-final { scan-tree-dump-times "v0_0_arg_vari_\[0-7\] \\(D\.\[0-9\]+, D\.\[0-9\]+\\);" 8 "gimple" } }  */
  PASS_ARGS_TO_ALL (p0, p0, p0);
/* { dg-final { scan-tree-dump-times "v0_0_arg_vari_\[0-7\] \\(D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+\\);" 8 "gimple" } }  */
}
#undef PASS_ARGS_TO_ALL

void v1_0_arg_vari_8(...) {}
void v1_0_arg_vari_9(...) {}
void v1_0_arg_vari_10(...) {}

/* First arg adjusted.  */

#pragma omp declare variant (v1_0_arg_vari_8) match (construct={dispatch}) adjust_args (need_device_ptr: 1:1)
void b_firstarg_needptr_literal_literal(...) {}

#pragma omp declare variant (v1_0_arg_vari_9) match (construct={dispatch}) adjust_args (need_device_ptr: 1:1) adjust_args (nothing: 2:omp_num_args)
void b_firstarg_needptr_literal_literal_rest_nothing_literal_numargs(...) {}

#pragma omp declare variant (v1_0_arg_vari_10) match (construct={dispatch}) adjust_args (nothing: 2:omp_num_args) adjust_args (need_device_ptr: 1:1)
void b_rest_nothing_literal_numargs_firstarg_needptr_literal_literal(...) {}

void do_first_arg(int *p1)
{
  int a = 42;
  /* 7 uses of p1.  */
  /* 7 uses of omp dispatch.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p1, D\.\[0-9\]+\\);" 7 "gimple" } }  */
  #pragma omp dispatch
  b_firstarg_needptr_literal_literal(p1);
  #pragma omp dispatch
  b_firstarg_needptr_literal_literal(p1, a);
  #pragma omp dispatch
  b_firstarg_needptr_literal_literal(p1, a, a);

  #pragma omp dispatch
  b_firstarg_needptr_literal_literal_rest_nothing_literal_numargs(p1, a);
  #pragma omp dispatch
  b_firstarg_needptr_literal_literal_rest_nothing_literal_numargs(p1, a, a);

  #pragma omp dispatch
  b_rest_nothing_literal_numargs_firstarg_needptr_literal_literal(p1, a);
  #pragma omp dispatch
  b_rest_nothing_literal_numargs_firstarg_needptr_literal_literal(p1, a, a);
/* { dg-final { scan-tree-dump-times "v1_0_arg_vari_1?\[089\] \\(D\.\[0-9\]+(?:, a){0,2}\\);" 7 "gimple" } }  */
}

/* Last arg adjusted.  */

void v2_0_arg_vari_11(...) {}
void v2_0_arg_vari_12(...) {}
void v2_0_arg_vari_13(...) {}
void v2_0_arg_vari_14(...) {}
void v2_0_arg_vari_15(...) {}
void v2_0_arg_vari_16(...) {}
void v2_0_arg_vari_17(...) {}
void v2_0_arg_vari_18(...) {}
void v2_0_arg_vari_19(...) {}
void v2_0_arg_vari_20(...) {}
void v2_0_arg_vari_21(...) {}
void v2_0_arg_vari_22(...) {}

#pragma omp declare variant (v2_0_arg_vari_11) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:)
void b_lastarg_needptr_numargs_default_0 (...) {}

#pragma omp declare variant (v2_0_arg_vari_12) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:)
void b_lastarg_needptr_numargs_default_1 (...) {}

#pragma omp declare variant (v2_0_arg_vari_13) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:)
void b_lastarg_needptr_numargs_default_2 (...) {}

// omp_num_args+/- : omp_num_args+/-

#pragma omp declare variant (v2_0_arg_vari_14) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args)
void b_lastarg_needptr_numargs_numargs_0_0 (...) {}

#pragma omp declare variant (v2_0_arg_vari_15) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args+0)
void b_lastarg_needptr_numargs_numargs_0_1 (...) {}

#pragma omp declare variant (v2_0_arg_vari_16) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args-0)
void b_lastarg_needptr_numargs_numargs_0_2 (...) {}

#pragma omp declare variant (v2_0_arg_vari_17) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args)
void b_lastarg_needptr_numargs_numargs_1_0 (...) {}

#pragma omp declare variant (v2_0_arg_vari_18) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args+0)
void b_lastarg_needptr_numargs_numargs_1_1 (...) {}

#pragma omp declare variant (v2_0_arg_vari_19) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args-0)
void b_lastarg_needptr_numargs_numargs_1_2 (...) {}

#pragma omp declare variant (v2_0_arg_vari_20) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args)
void b_lastarg_needptr_numargs_numargs_2_0 (...) {}

#pragma omp declare variant (v2_0_arg_vari_21) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args+0)
void b_lastarg_needptr_numargs_numargs_2_1 (...) {}

#pragma omp declare variant (v2_0_arg_vari_22) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args-0)
void b_lastarg_needptr_numargs_numargs_2_2 (...) {}

/* 12 function calls.  */
#define PASS_ARGS_TO_ALL(...) \
  do {								\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_default_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_default_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_default_2(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_0_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_0_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_0_2(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_1_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_1_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_1_2(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_2_0(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_2_1(__VA_ARGS__);		\
    _Pragma("omp dispatch")					\
    b_lastarg_needptr_numargs_numargs_2_2(__VA_ARGS__);		\
  } while (0)

void do_lastarg_0(int *p2)
{
  int a = 42;
  /* 3 uses of p2, times 12, 3*12=36.  */
  /* 3 expansions, times 12, 3*12=36 uses of omp dispatch.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p2, D\.\[0-9\]+\\);" 36 "gimple" } }  */
  PASS_ARGS_TO_ALL (p2);
  PASS_ARGS_TO_ALL (a, p2);
  PASS_ARGS_TO_ALL (a, a, p2);
/* { dg-final { scan-tree-dump-times "v2_0_arg_vari_\[12\]\[0-9\] \\((?:a, ){0,2}D\.\[0-9\]+\\);" 36 "gimple" } }  */
}

#undef PASS_ARGS_TO_ALL

void v3_0_arg_vari_23(...) {}
void v3_0_arg_vari_24(...) {}
void v3_0_arg_vari_25(...) {}
void v3_0_arg_vari_26(...) {}
void v3_0_arg_vari_27(...) {}
void v3_0_arg_vari_28(...) {}
void v3_0_arg_vari_29(...) {}
void v3_0_arg_vari_30(...) {}
void v3_0_arg_vari_31(...) {}
void v3_0_arg_vari_32(...) {}
void v3_0_arg_vari_33(...) {}
void v3_0_arg_vari_34(...) {}
void v3_0_arg_vari_35(...) {}
void v3_0_arg_vari_36(...) {}
void v3_0_arg_vari_37(...) {}
void v3_0_arg_vari_38(...) {}
void v3_0_arg_vari_39(...) {}
void v3_0_arg_vari_40(...) {}
void v3_0_arg_vari_41(...) {}
void v3_0_arg_vari_42(...) {}
void v3_0_arg_vari_43(...) {}
void v3_0_arg_vari_44(...) {}
void v3_0_arg_vari_45(...) {}
void v3_0_arg_vari_46(...) {}

#pragma omp declare variant (v3_0_arg_vari_23) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_24) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_25) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_2 (...) {}

// omp_num_args+/- : omp_num_args+/-

#pragma omp declare variant (v3_0_arg_vari_26) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_27) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args+0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_28) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:omp_num_args-0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_2 (...) {}

#pragma omp declare variant (v3_0_arg_vari_29) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_30) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args+0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_31) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args-0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_2 (...) {}

#pragma omp declare variant (v3_0_arg_vari_32) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_33) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args+0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_34) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args-0) adjust_args (nothing: 1:omp_num_args-1)
void b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_2 (...) {}


/* same as above section with clauses reversed */

#pragma omp declare variant (v3_0_arg_vari_35) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args:)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_36) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args+0:)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_37) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args-0:)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_2 (...) {}

// omp_num_args+/- : omp_num_args+/-

#pragma omp declare variant (v3_0_arg_vari_38) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args:omp_num_args)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_39) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args:omp_num_args+0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_40) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args:omp_num_args-0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_2 (...) {}

#pragma omp declare variant (v3_0_arg_vari_41) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_42) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args+0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_43) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args+0:omp_num_args-0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_2 (...) {}

#pragma omp declare variant (v3_0_arg_vari_44) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_0 (...) {}

#pragma omp declare variant (v3_0_arg_vari_45) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args+0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_1 (...) {}

#pragma omp declare variant (v3_0_arg_vari_46) match (construct={dispatch}) adjust_args (nothing: 1:omp_num_args-1) adjust_args (need_device_ptr: omp_num_args-0:omp_num_args-0)
void b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_2 (...) {}

/* 24 function calls. */
#define PASS_ARGS_TO_ALL(...) \
  do {												\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_default_rest_nothing_literal_numargs_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_0_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_1_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_lastarg_needptr_numargs_numargs_rest_nothing_literal_numargs_2_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_default_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_0_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_1_2 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_0 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_1 (__VA_ARGS__);		\
    _Pragma("omp dispatch")									\
    b_rest_nothing_literal_numargs_lastarg_needptr_numargs_numargs_2_2 (__VA_ARGS__);		\
  } while (0)

void do_lastarg_1(int *p3)
{
  int a = 42;
  /* 3 uses of p3, times 24, 3*24=72.  */
  /* 3 expansions, times 24, 3*24=72 uses of omp dispatch.  */
  /* Can't pass a single arg to this one.  */
  PASS_ARGS_TO_ALL (a, p3);
  PASS_ARGS_TO_ALL (a, a, p3);
  PASS_ARGS_TO_ALL (a, a, a, p3);
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p3, D\.\[0-9\]+\\);" 72 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "v3_0_arg_vari_\[2-4\]\[0-9\] \\((?:a, ){1,3}D\.\[0-9\]+\\);" 72 "gimple" } }  */
}
#undef PASS_ARGS_TO_ALL

/* 24 + 7 + 36 + 72 = 139.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 139 "gimple" } }  */


/* Lots of these cases are invalid (depending on the value of the literal),
   thus they go somewhere else.  */

// omp_num_args+/- : literal

// #pragma omp declare variant (N/A) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args:1)
// void b_numargs_literal_0 (...) {}

// #pragma omp declare variant (N/A) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args+0:1)
// void b_numargs_literal_1 (...) {}

// #pragma omp declare variant (N/A) match (construct={dispatch}) adjust_args (need_device_ptr: omp_num_args-0:1)
// void b_numargs_literal_2 (...) {}
