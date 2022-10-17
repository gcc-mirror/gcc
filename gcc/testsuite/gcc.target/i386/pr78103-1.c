/* PR target/78103 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-lzcnt" } */
/* { dg-final { scan-assembler-not {\mcltq\M} } } */

long long
foo (long long x)
{
  return __builtin_clzll (x);
}

long long
bar (long long x)
{
  return (unsigned int) __builtin_clzll (x);
}

long long
baz (int x)
{
  return __builtin_clz (x);
}

long long
qux (int x)
{
  return (unsigned int) __builtin_clz (x);
}
