/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=ivybridge" } */
long long
foo(long long x, unsigned bits)
{
  return x + (unsigned) __builtin_ctz(bits);
}
/* { dg-final { scan-assembler-not "cltq" } } */
