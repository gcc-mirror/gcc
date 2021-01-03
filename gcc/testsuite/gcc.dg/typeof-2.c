/* Test qualifier preservation of typeof and discarded for __auto_type. */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

/* Check that the qualifiers are preserved for atomic types. */

extern int i;

extern int * p;

extern int _Atomic const ci;
extern __typeof (ci) ci;

extern int _Atomic volatile vi;
extern __typeof (vi) vi;

extern int * _Atomic restrict ri;
extern __typeof (ri) ri;

/* Check that the qualifiers are discarded for atomic types. */

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

/* Check that the qualifiers are discarded for non-atomic types. */

void g(void)
{
  __auto_type aci = nci;
  int *paci = &aci;

  __auto_type avi = nvi;
  int *pavi = &avi;

  __auto_type ari = nri;
  int **pari = &ari;
}
