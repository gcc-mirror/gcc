/* Test MIPS32 DSP REV 2 MULT instruction.  Tune for a CPU that has
   pipelined mult.  */
/* { dg-do compile } */
/* { dg-options "-mgp32 -mdspr2 -O2 -mtune=74kc" } */

/* See PR target/51729 for the reason behind the XFAILs.  */
/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler "ac1" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "ac2" { xfail *-*-* } } } */

typedef long long a64;

NOMIPS16 a64 test (a64 *a, int *b, int *c)
{
  a[0] = (a64) b[0] * c[0];
  a[1] = (a64) b[1] * c[1];
}
