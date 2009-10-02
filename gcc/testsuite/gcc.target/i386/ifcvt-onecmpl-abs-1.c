/* { dg-do compile } */
/* This test checks for if-conversion of one's complement
 * abs function.  */
/* { dg-options "-O -mtune=generic" } */
/* { dg-final { scan-assembler "sar" } } */
/* { dg-final { scan-assembler "xor" } } */

/* Check code generation for one's complement version of abs */

int onecmplabs(int x)
{
  if (x < 0)
    x = ~x;
  return x;
}
