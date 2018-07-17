/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -falign-functions=16" } */
/* { dg-final { scan-assembler ".p2align 5" } } */

void f(double *a, double *b, double *c, unsigned long n) {
  unsigned long i;
  for (i=0; i < n; i++)
    a[i] = b[i] + c[i];
}
