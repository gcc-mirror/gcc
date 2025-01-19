/* PR target/36503 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "movl\[ \\t\]+\\\$32" } } */

int foo (int i, int n)
{
  return i << (32 - n);
}

int bar (int i, int n)
{
  return i >> (32 - n);
}

unsigned int baz (unsigned int i, int n)
{
  return i >> (32 - n);
}
