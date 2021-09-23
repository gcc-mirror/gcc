/* PR target/78103 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-lzcnt" } */
/* { dg-final { scan-assembler-not {\mmovl\M} } } */
/* { dg-final { scan-assembler-not {\mxor[lq]\M} } } */
/* { dg-final { scan-assembler-not {\msubl\M} } } */
/* { dg-final { scan-assembler {\m(leal|addl|incl)\M} } } */

unsigned int
foo (unsigned int x)
{
  return __CHAR_BIT__ * sizeof (unsigned int) - __builtin_clz (x);
}

unsigned int
bar (unsigned int x)
{
  return __CHAR_BIT__ * sizeof (unsigned int) - 1 - __builtin_clz (x);
}

#ifdef __x86_64__
unsigned int
baz (unsigned long long x)
{
  return __CHAR_BIT__ * sizeof (unsigned long long) - __builtin_clzll (x);
}

unsigned int
qux (unsigned long long x)
{
  return __CHAR_BIT__ * sizeof (unsigned long long) - 1 - __builtin_clzll (x);
}
#endif
