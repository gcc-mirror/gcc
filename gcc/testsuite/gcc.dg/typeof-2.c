/* Test qualifier discard of typeof for atomic types. */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

/* Check that the qualifiers are discarded for atomic types. */

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

/* Check that the qualifiers are preserved for non-atomic types. */

extern int const j;

extern int volatile k;

extern int * restrict q;

extern int const nci;
extern __typeof (nci) j;

extern int volatile nvi;
extern __typeof (nvi) k;

extern int * restrict nri;
extern __typeof (nri) q;

void g(void)
{
  __auto_type aci = nci;
  int const *paci = &aci;

  __auto_type avi = nvi;
  int volatile *pavi = &avi;

  __auto_type ari = nri;
  int * restrict *pari = &ari;
}
