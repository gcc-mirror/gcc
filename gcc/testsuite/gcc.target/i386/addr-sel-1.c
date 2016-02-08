/* PR rtl-optimization/28940 */
/* Origin: Lev Makhlis <lmakhlis@bmc.com> */

/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -mtune=i686" } */

char a[10], b[10];

int f(int i)
{
  return a[i+1] + b[i+1];
}

/* { dg-final { scan-assembler "a\\+1" } } */
/* { dg-final { scan-assembler "b\\+1" { xfail *-*-* } } } */
