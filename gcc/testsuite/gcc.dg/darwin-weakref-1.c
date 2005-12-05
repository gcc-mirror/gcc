/* { dg-do compile { target *-*-darwin* } } */
// { dg-require-weak "" }
// { dg-options "-O2" }
// { dg-options "-O2 -mmacosx-version-min=10.2" { target { powerpc-*-darwin* } } }
/* { dg-final { scan-assembler "weak_reference _wv1" } } */
/* { dg-final { scan-assembler "weak_reference _wf1" } } */
/* { dg-final { scan-assembler-not "weak_reference _w.2" } } */


typedef int vtype;

extern vtype wv1;
static vtype Wv1a __attribute__((weakref ("wv1")));
vtype *pv1a = &Wv1a;

extern vtype wv2;
static vtype Wv2a __attribute__((weakref ("wv2")));
vtype *pv2a = &wv2;

typedef void ftype(void);

extern ftype wf1;
static ftype Wf1a __attribute__((weakref ("wf1")));
ftype *pf1a = &Wf1a;

extern ftype wf2;
static ftype Wf2a __attribute__((weakref ("wf2")));
ftype *pf2a = &wf2;
