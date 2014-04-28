/* { dg-skip-if "need at least armv5te" { *-*-* } { "-march=armv[234]*" "-mthumb" } { "" } } */
/* { dg-options "-O2 -march=armv5te -marm" } */
/* { dg-final { scan-assembler "bx" } } */
/* { dg-final { scan-assembler-not "blx" } } */

int lcal (int) __attribute__ ((long_call));

int
dec (int a)
{
  return lcal (a);
}
