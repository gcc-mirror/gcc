/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */
/* { dg-final { scan-assembler-times "vpxor" 2 } } */
/* { dg-final { scan-assembler-times "vpcmpeq" 2 } } */

typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef char v16qi __attribute__((vector_size(16)));
typedef char v32qi __attribute__((vector_size(32)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef double v2df __attribute__((vector_size(16)));
typedef double v4df __attribute__((vector_size(32)));

v16qi b1;
v8hi h1;
v4si s1;
v2di l1;
v4sf f1;
v2df d1;
v32qi b2;
v16hi h2;
v8si s2;
v4di l2;
v8sf f2;
v4df d2;

void
foo ()
{
  d1 = __extension__(v2df){0, 0};
  f1 = __extension__(v4sf){0, 0, 0};
  l1 = __extension__(v2di){0, 0};
  s1 = __extension__(v4si){0, 0, 0, 0};
  h1 = __extension__(v8hi){0, 0, 0, 0, 0, 0, 0, 0};
  b1 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  h2 = __extension__(v16hi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
}

void
foo1 ()
{
  s1 = __extension__(v4si){-1, -1, -1, -1};
  h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
  b1 = __extension__(v16qi){-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
}


void
foo2 ()
{
  d2 = __extension__(v4df){0, 0, 0, 0};
  f2 = __extension__(v8sf){0, 0, 0, 0, 0, 0, 0, 0};
  l2 = __extension__(v4di){0, 0, 0, 0};
  s2 = __extension__(v8si){0, 0, 0, 0, 0, 0, 0, 0};
  h1 = __extension__(v8hi){0, 0, 0, 0, 0, 0, 0, 0};
  b1 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  b2 = __extension__(v32qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
}

void
foo3 ()
{
  s2 = __extension__(v8si){-1, -1, -1, -1, -1, -1, -1, -1};
  h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
  b1 = __extension__(v16qi){-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
}
