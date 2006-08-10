/* Check that %? in inline asm expands to nothing.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm32 } */
int b;
int foo(int a)
{
  if (a)
    b = 42;
  asm ("test%?me":"=r"(a):"0"(a));
  return a;
}
/* { dg-final { scan-assembler "testme" } } */
