/* { dg-do compile } */
/* { dg-options "-O2 -fpic -fno-plt" } */
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

/* { dg-final { scan-assembler-times "#:got:" 2 { target { aarch64_tiny || aarch64_small } } } } */
/* { dg-final { scan-assembler-times "#:got_lo12:" 2 { target aarch64_small } } } */
