/* { dg-do compile } */
/* { dg-mips-options "-march=octeon -mgp64" } */
/* { dg-final { scan-assembler-not "\tdmul" } } */

NOMIPS16 long long
f (long long a)
{
  return a * 7;
}
