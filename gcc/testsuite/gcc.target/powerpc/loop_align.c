/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -falign-functions=16" } */
/* { dg-final { scan-assembler ".p2align 5" } } */

void f(double *a, double *b, double *c, unsigned long n) {
  unsigned long i;
  for (i=0; i < n; i++)
    a[i] = b[i] + c[i];
}
