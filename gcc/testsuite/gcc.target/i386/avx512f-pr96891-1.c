/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-not "%k\[0-7\]" } } */

typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));

#define FOO(VTYPE, OPNAME, OP)			\
  VTYPE						\
  foo_##VTYPE##_##OPNAME (VTYPE a, VTYPE b)	\
  {						\
    return a OP b;				\
  }						\

FOO (v4sf, eq, ==)
FOO (v4sf, neq, !=)
FOO (v4sf, gt, >)
FOO (v4sf, ge, >=)
FOO (v4sf, lt, <)
FOO (v4sf, le, <=)
FOO (v8sf, eq, ==)
FOO (v8sf, neq, !=)
FOO (v8sf, gt, >)
FOO (v8sf, ge, >=)
FOO (v8sf, lt, <)
FOO (v8sf, le, <=)
FOO (v2df, eq, ==)
FOO (v2df, neq, !=)
FOO (v2df, gt, >)
FOO (v2df, ge, >=)
FOO (v2df, lt, <)
FOO (v2df, le, <=)
FOO (v4df, eq, ==)
FOO (v4df, neq, !=)
FOO (v4df, gt, >)
FOO (v4df, ge, >=)
FOO (v4df, lt, <)
FOO (v4df, le, <=)
