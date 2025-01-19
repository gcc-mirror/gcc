/* PR target/36503 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movl\[ \\t\]+\\\$64" } } */

long long foo (long long i, int n)
{
  return i << (64 - n);
}

long long bar (long long i, int n)
{
  return i >> (64 - n);
}

unsigned long long baz (unsigned long long i, int n)
{
  return i >> (64 - n);
}
