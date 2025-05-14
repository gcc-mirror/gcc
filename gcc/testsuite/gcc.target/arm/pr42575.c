/* { dg-options "-O2" }  */
/* { dg-skip-if "Thumb1 lacks UMULL" { arm_thumb1 } } */
/* Make sure RA does good job allocating registers and avoids
   unnecessary moves.  */
/* { dg-final { scan-assembler-not "mov" } } */

long long longfunc(long long x, long long y)
{
      return x * y;
}
