/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "mov" } } */

int foo()
{
  int (**bar)(void);
  asm("":"=a"(bar));
  return (*bar)();
}
