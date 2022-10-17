/* PR target/78103 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-lzcnt" } */
/* { dg-final { scan-assembler-not {\mmovl\M} } } */
/* { dg-final { scan-assembler-not {\mmovslq\M} } } */
/* { dg-final { scan-assembler-not {\mxor[lq]\M} } } */
/* { dg-final { scan-assembler-not {\msubq\M} } } */
/* { dg-final { scan-assembler {\m(leaq|addq|incq)\M} { target { ! x32 } } } } */
/* { dg-final { scan-assembler {\m(leal|addl|incl)\M} { target x32 } } } */

unsigned long long
foo (unsigned int x)
{
  return __CHAR_BIT__ * sizeof (unsigned int) - __builtin_clz (x);
}

unsigned long long
bar (unsigned int x)
{
  return __CHAR_BIT__ * sizeof (unsigned int) - 1 - __builtin_clz (x);
}

unsigned long long
baz (unsigned long long x)
{
  return __CHAR_BIT__ * sizeof (unsigned long long) - __builtin_clzll (x);
}

unsigned long long
qux (unsigned long long x)
{
  return __CHAR_BIT__ * sizeof (unsigned long long) - 1 - __builtin_clzll (x);
}
