/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2 -mno-popcnt" } */
/* { dg-final { scan-assembler "setp" } } */
/* { dg-final { scan-assembler "jnp" } } */
/* { dg-final { scan-assembler "jp" } } */

void dummy(void);

int foo(unsigned long long x)
{
  return !__builtin_parityll(x);
}

void bar(unsigned long long x)
{
  if (__builtin_parityll(x))
    dummy();
}

void baz(unsigned long long x)
{
  if (!__builtin_parityll(x))
    dummy();
}
