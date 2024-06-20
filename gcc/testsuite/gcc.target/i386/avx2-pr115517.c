/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vpsrad" 2 } } */
/* { dg-final { scan-assembler-times "vpsraw" 2 } } */

typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));

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

v4si
foo3 (v4si a)
{
  return a < __extension__(v4si) { 0, 0, 0, 0};
}

v8si
foo4 (v8si a)
{
  return a < __extension__(v8si) { 0, 0, 0, 0, 0, 0, 0, 0};
}
