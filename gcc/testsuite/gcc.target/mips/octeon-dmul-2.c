/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "using DMUL is no worse size-wise, and can be better if the constant is used elsewhere" { *-*-* } { "-Os" } { "" } } */
/* { dg-final { scan-assembler-not "\tdmul" } } */

NOMIPS16 long long
f (long long a)
{
  return a * 7;
}
