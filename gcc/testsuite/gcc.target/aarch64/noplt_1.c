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

/* { dg-final { scan-assembler "#:got:" { target { aarch64_tiny || aarch64_small } } } } */
/* { dg-final { scan-assembler "#:got_lo12:" { target aarch64_small } } } */
