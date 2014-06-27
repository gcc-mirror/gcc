/* Test qualifier discard of typeof for atomic types. */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

extern int i;

extern int * p;

extern int _Atomic const ci;
extern __typeof (ci) i;

extern int _Atomic volatile vi;
extern __typeof (vi) i;

extern int * _Atomic restrict ri;
extern __typeof (ri) p;

void f(void)
{
  __auto_type aci = ci;
  int *paci = &aci;

  __auto_type avi = vi;
  int *pavi = &avi;

  __auto_type ari = ri;
  int **pari = &ari;
}
