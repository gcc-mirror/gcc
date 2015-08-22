/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-require-effective-target alloca } */

/* These are crash tests related to PR middle-end/6994; see also
   g++.dg/ext/vla1.C.  Note that at present A and C cannot be inlined.  */

static inline void A (int i)
{
  struct S { int ar[1][i]; } s;

  s.ar[0][0] = 0;
}

void B(void)
{
  A(23);
}

static inline void C (int i)
{
  union U { int ar[1][i]; } u;

  u.ar[0][0] = 0;
}

void D(void)
{
  C(23);
}
