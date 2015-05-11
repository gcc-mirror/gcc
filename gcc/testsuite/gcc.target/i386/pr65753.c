/* { dg-do compile } */
/* { dg-options "-fPIC -O" } */
/* { dg-final { scan-assembler-not "call" } } */

void foo(void (*bar)(void))
{
  bar();
}
