/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2 -mno-popcnt" } */
/* { dg-final { scan-assembler "setp" } } */
/* { dg-final { scan-assembler "jnp" } } */
/* { dg-final { scan-assembler "jp" } } */

void dummy(void);

int foo(unsigned int x)
{
  return !__builtin_parity(x);
}

void bar(unsigned int x)
{
  if (__builtin_parity(x))
    dummy();
}

void baz(unsigned int x)
{
  if (!__builtin_parity(x))
    dummy();
}
