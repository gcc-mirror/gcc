/* { dg-do compile } */
/* { dg-options " " } */
/* { dg-final { scan-assembler-not "slli" } } */

int x;

void foo(void)
{
  x <<= 1;
}
