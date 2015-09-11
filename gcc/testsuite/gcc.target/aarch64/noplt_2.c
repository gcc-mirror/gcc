/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-skip-if "-mcmodel=large, no support for -fpic" { aarch64-*-* }  { "-mcmodel=large" } { "" } } */

__attribute__ ((noplt))
int* bar0 (void) ;
int* bar1 (void) ;

int
foo (int a)
{
  int *b0 = bar0 ();
  int *b1 = bar1 ();
  return b0[a] + b1[a];
}

/* { dg-final { scan-assembler-times "blr" 1 } } */
/* { dg-final { scan-assembler-times "bl\t" 1 } } */
