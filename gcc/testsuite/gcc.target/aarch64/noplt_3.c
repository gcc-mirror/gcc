/* { dg-do compile } */
/* { dg-options "-O2 -fpic -fno-plt" } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "-mcmodel=large, no support for -fpic" { aarch64-*-* }  { "-mcmodel=large" } { "" } } */

int dec (int);

int
cal (int a)
{
  return dec (a);
}

void
cal_novalue (int a)
{
  dec (a);
}

/* { dg-final { scan-assembler-times "br\t" 2 } } */
/* { dg-final { scan-assembler-not "b\t" } } */
