/* { dg-do compile } */
/* { dg-options "-Ofast" } */

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

void
foo (int32x2_t *foo, int32x2_t *bar)
{
  int i = 0;
  int32x2_t val = { 3, 2 };

  for (i = 0; i < 1024; i+=2)
    foo[i] = bar[i] + bar[i + 1];
}

/* Xfail for now since IVOPT doesn't choose [base+offset] addressing mode.
   See PR79213.  */
/* { dg-final { scan-assembler "ldp\td\[0-9\]+, d\[0-9\]" { xfail *-*-* } } } */
