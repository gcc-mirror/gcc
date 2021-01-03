/* { dg-do compile { target ia32 } } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -O2 -mregparm=3" } */
/* { dg-final { scan-assembler-not "call" } } */

void foo(void (*bar)(void))
{
  bar();
}
