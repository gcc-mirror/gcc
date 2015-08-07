/* { dg-do compile } */
/* { dg-options "-O2 -fpic -fno-plt" } */
/* { dg-skip-if "-mcmodel=large, no support for -fpic" { aarch64-*-* }  { "-mcmodel=large" } { "" } } */

int* bar (void) ;

int
foo (int a)
{
  int *b = bar ();
  return b[a];
}

/* { dg-final { scan-assembler "blr" } } */
/* { dg-final { scan-assembler-not "bl\t" } } */
