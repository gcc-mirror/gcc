/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpsrad" 3 } } */
/* { dg-final { scan-assembler-times "vpsraw" 3 } } */
/* { dg-final { scan-assembler-times "vpsraq" 3 } } */

typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef short v32hi __attribute__((vector_size(64)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));
typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));
typedef long long v8di __attribute__((vector_size(64)));

v8hi
foo (v8hi a)
{
  return a < __extension__(v8hi) { 0, 0, 0, 0, 0, 0, 0, 0};
}

v16hi
foo2 (v16hi a)
{
  return a < __extension__(v16hi) { 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0};
}

v32hi
foo3 (v32hi a)
{
  return a < __extension__(v32hi) { 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0};
}

v4si
foo4 (v4si a)
{
  return a < __extension__(v4si) { 0, 0, 0, 0};
}

v8si
foo5 (v8si a)
{
  return a < __extension__(v8si) { 0, 0, 0, 0, 0, 0, 0, 0};
}

v16si
foo6 (v16si a)
{
  return a < __extension__(v16si) { 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0};
}

v2di
foo7 (v2di a)
{
  return a < __extension__(v2di) { 0, 0};
}

v4di
foo8 (v4di a)
{
  return a < __extension__(v4di) { 0, 0, 0, 0};
}

v8di
foo9 (v8di a)
{
  return a < __extension__(v8di) { 0, 0, 0, 0, 0, 0, 0, 0};
}
