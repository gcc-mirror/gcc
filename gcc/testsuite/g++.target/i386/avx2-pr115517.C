/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vpsrlq" 2 } } */
/* { dg-final { scan-assembler-times "vpsrld" 2 } } */
/* { dg-final { scan-assembler-times "vpsrlw" 2 } } */

typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));

v8hi
foo (v8hi a)
{
  v8hi const1_op = __extension__(v8hi){1,1,1,1,1,1,1,1};
  v8hi const0_op = __extension__(v8hi){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v16hi
foo2 (v16hi a)
{
  v16hi const1_op = __extension__(v16hi){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  v16hi const0_op = __extension__(v16hi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4si
foo3 (v4si a)
{
  v4si const1_op = __extension__(v4si){1,1,1,1};
  v4si const0_op = __extension__(v4si){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v8si
foo4 (v8si a)
{
  v8si const1_op = __extension__(v8si){1,1,1,1,1,1,1,1};
  v8si const0_op = __extension__(v8si){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v2di
foo3 (v2di a)
{
  v2di const1_op = __extension__(v2di){1,1};
  v2di const0_op = __extension__(v2di){0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4di
foo4 (v4di a)
{
  v4di const1_op = __extension__(v4di){1,1,1,1};
  v4di const0_op = __extension__(v4di){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}
