/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -O2" } */
/* { dg-final { scan-assembler-not "call" } } */

void foo(void (*bar)(void))
{
  bar();
}
