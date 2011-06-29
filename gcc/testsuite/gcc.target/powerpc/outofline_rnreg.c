/* Test that registers used by out of line restore functions does not get renamed.
   AIX, and 64 bit targets uses r1, which rnreg stays away from.
   Linux 32 bits targets uses r11, which is susceptible to be renamed */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Os -frename-registers -fdump-rtl-rnreg" } */
/* "* renamed" or "* no available better choice" results are not acceptable */
/* { dg-final { scan-rtl-dump-not "Register 11 in insn *" "rnreg" { target powerpc*-*-linux* } } } */
/* { dg-final { cleanup-rtl-dump "rnreg" } } */
int
calc (int j)
{
  if (j<=1) return 1;
  return calc(j-1)*(j+1);
}
