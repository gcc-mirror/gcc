/* { dg-do compile } */
/* { dg-options "-mlasx -O2" } */
/* { dg-final { scan-assembler-times "vsrli.b" 2 } } */
/* { dg-final { scan-assembler-times "vsrli.h" 2 } } */
/* { dg-final { scan-assembler-times "vsrli.w" 2 } } */
/* { dg-final { scan-assembler-times "vsrli.d" 2 } } */
/* { dg-final { scan-assembler-times "vsrai.b" 2 } } */
/* { dg-final { scan-assembler-times "vsrai.h" 2 } } */
/* { dg-final { scan-assembler-times "vsrai.w" 2 } } */
/* { dg-final { scan-assembler-times "vsrai.d" 2 } } */

typedef signed char v16qi __attribute__((vector_size(16)));
typedef signed char v32qi __attribute__((vector_size(32)));
typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));

v16qi
foo (v16qi a)
{
  v16qi const1_op = __extension__(v16qi){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  v16qi const0_op = __extension__(v16qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v32qi
foo2 (v32qi a)
{
  v32qi const1_op = __extension__(v32qi){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  v32qi const0_op = __extension__(v32qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v8hi
foo3 (v8hi a)
{
  v8hi const1_op = __extension__(v8hi){1,1,1,1,1,1,1,1};
  v8hi const0_op = __extension__(v8hi){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v16hi
foo4 (v16hi a)
{
  v16hi const1_op = __extension__(v16hi){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  v16hi const0_op = __extension__(v16hi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4si
foo5 (v4si a)
{
  v4si const1_op = __extension__(v4si){1,1,1,1};
  v4si const0_op = __extension__(v4si){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v8si
foo6 (v8si a)
{
  v8si const1_op = __extension__(v8si){1,1,1,1,1,1,1,1};
  v8si const0_op = __extension__(v8si){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v2di
foo7 (v2di a)
{
  v2di const1_op = __extension__(v2di){1,1};
  v2di const0_op = __extension__(v2di){0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4di
foo8 (v4di a)
{
  v4di const1_op = __extension__(v4di){1,1,1,1};
  v4di const0_op = __extension__(v4di){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v16qi
foo9 (v16qi a)
{
  v16qi const1_op = __extension__(v16qi){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
  v16qi const0_op = __extension__(v16qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v32qi
foo10 (v32qi a)
{
  v32qi const1_op = __extension__(v32qi){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
  v32qi const0_op = __extension__(v32qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v8hi
foo11 (v8hi a)
{
  v8hi const1_op = __extension__(v8hi){-1,-1,-1,-1,-1,-1,-1,-1};
  v8hi const0_op = __extension__(v8hi){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v16hi
foo12 (v16hi a)
{
  v16hi const1_op = __extension__(v16hi){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
  v16hi const0_op = __extension__(v16hi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4si
foo13 (v4si a)
{
  v4si const1_op = __extension__(v4si){-1,-1,-1,-1};
  v4si const0_op = __extension__(v4si){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v8si
foo14 (v8si a)
{
  v8si const1_op = __extension__(v8si){-1,-1,-1,-1,-1,-1,-1,-1};
  v8si const0_op = __extension__(v8si){0,0,0,0,0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}

v2di
foo15 (v2di a)
{
  v2di const1_op = __extension__(v2di){-1,-1};
  v2di const0_op = __extension__(v2di){0,0};
  return a < const0_op ? const1_op : const0_op;
}

v4di
foo16 (v4di a)
{
  v4di const1_op = __extension__(v4di){-1,-1,-1,-1};
  v4di const0_op = __extension__(v4di){0,0,0,0};
  return a < const0_op ? const1_op : const0_op;
}
