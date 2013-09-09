/* { dg-do compile } */
/* { dg-options "-O2 -mno-sdata" } */

int i;

int f (void)
{
  return i;
}
/* { dg-final { scan-assembler-not "@sda" } } */
