/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "vpcmpeq" 4 } } */
/* { dg-final { scan-assembler-not {(?n)%k[0-9]} } } */

typedef char v16qi __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));

v16qi
foo (v16qi a)
{
  v16qi b = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  return a == b;
}

v8hi
foo2 (v8hi a)
{
  v8hi b = {0, 0, 0, 0, 0, 0, 0, 0};
  return a == b;
}

v4si
foo3 (v4si a)
{
  v4si b = {0, 0, 0, 0};
  return a == b;
}

v2di
foo4 (v2di a)
{
  v2di b = {0, 0};
  return a == b;
}

