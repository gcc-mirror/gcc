/* PR ipa/65521 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

struct S { int s; };
int f6 (void *, unsigned long);
int f7 (int, int *, unsigned long);
int f8 (void);
int f9 (void (*) (void));

int
f1 (void *p)
{
  return f6 (p, 256UL);
}

int
f2 (void *p)
{
  return f6 (p, 256UL);
}

int
f3 (struct S *x)
{
  return f7 (f8 (), &x->s, 16UL);
}

int
f4 (struct S *x)
{
  return f7 (f8 (), &x->s, 16UL);
}

void
f5 (void)
{
  f9 (f5);
}
