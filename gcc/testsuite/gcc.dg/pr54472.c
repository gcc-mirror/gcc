/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O -fschedule-insns -fselective-scheduling" } */

int main ()
{
  int a[3][3][3];
  __builtin_memset (a, 0, sizeof a);
  return 0;
}
