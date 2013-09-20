/* { dg-do compile } */
/* { dg-options "-O2 -msdata" } */

int i;

int f (void)
{
  return i;
}
/* { dg-final { scan-assembler "@sda" } } */
