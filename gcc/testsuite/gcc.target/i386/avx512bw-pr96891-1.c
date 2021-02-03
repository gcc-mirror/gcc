/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-not "%k\[0-7\]" } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef long long v4di __attribute__ ((vector_size (32)));

#define FOO(VTYPE, OPNAME, OP)			\
  VTYPE						\
  foo_##VTYPE##_##OPNAME (VTYPE a, VTYPE b)	\
  {						\
    return a OP b;				\
  }						\

#define FOO1(VTYPE, OPNAME, OP)			\
  VTYPE						\
  foo1_##VTYPE##_##OPNAME (VTYPE a, VTYPE b)	\
  {						\
    return ~(a OP b);				\
  }						\

FOO (v16qi, eq, ==)
FOO1 (v16qi, neq, !=)
FOO (v16qi, gt, >)
FOO (v16qi, lt, <)
FOO1 (v16qi, le, <=)
FOO1 (v16qi, ge, >=)
FOO (v32qi, eq, ==)
FOO1 (v32qi, neq, !=)
FOO (v32qi, gt, >)
FOO (v32qi, lt, <)
FOO1 (v32qi, le, <=)
FOO1 (v32qi, ge, >=)
FOO (v8hi, eq, ==)
FOO1 (v8hi, neq, !=)
FOO (v8hi, gt, >)
FOO (v8hi, lt, <)
FOO1 (v8hi, le, <=)
FOO1 (v8hi, ge, >=)
FOO (v16hi, eq, ==)
FOO1 (v16hi, neq, !=)
FOO (v16hi, gt, >)
FOO (v16hi, lt, <)
FOO1 (v16hi, le, <=)
FOO1 (v16hi, ge, >=)
FOO (v4si, eq, ==)
FOO1 (v4si, neq, !=)
FOO (v4si, gt, >)
FOO (v4si, lt, <)
FOO1 (v4si, le, <=)
FOO1 (v4si, ge, >=)
FOO (v8si, eq, ==)
FOO1 (v8si, neq, !=)
FOO (v8si, gt, >)
FOO (v8si, lt, <)
FOO1 (v8si, le, <=)
FOO1 (v8si, ge, >=)
FOO (v2di, eq, ==)
FOO1 (v2di, neq, !=)
FOO (v2di, gt, >)
FOO (v2di, lt, <)
FOO1 (v2di, le, <=)
FOO1 (v2di, ge, >=)
FOO (v4di, eq, ==)
FOO1 (v4di, neq, !=)
FOO (v4di, gt, >)
FOO (v4di, lt, >)
FOO1 (v4di, le, <=)
FOO1 (v4di, ge, >=)
