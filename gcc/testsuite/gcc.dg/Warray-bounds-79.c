/* PR tree-optimization/99475 - bogus -Warray-bounds accessing an array
   element of empty structs
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct S
{
#if SOME_CONFIG_MACRO
  /* Suppose the contents are empty in the development configuration
     but non-empty in others.  Out of bounds accesses to elements of
     the arrays below should be diagnosed in all configurations,
     including when S is empty, even if they are folded away.  */
  int member;
#endif
};

extern struct S sa3[3];
extern struct S sa2_3[2][3];
extern struct S sa3_4_5[3][4][5];

void sink (void*);


void access_sa3 (void)
{
  sa3[0] = (struct S){ };
  sa3[1] = (struct S){ };
  sa3[2] = (struct S){ };
  sa3[3] = (struct S){ };       // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
}

void access_sa3_ptr (void)
{
  struct S *p = &sa3[0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[3] = (struct S){ };         // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
}

void access_sa2_3_ptr (void)
{
  struct S *p = &sa2_3[0][0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[6] = (struct S){ };         // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
}

void access_sa3_4_5_ptr (struct S s, int i)
{
  struct S *p = &sa3_4_5[0][0][0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[60] = (struct S){ };        // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
}


void access_vla3 (struct S s, unsigned n)
{
  struct S vla3[3 < n ? 3 : n];

  vla3[0] = (struct S){ };
  vla3[1] = (struct S){ };
  vla3[2] = (struct S){ };
  vla3[3] = (struct S){ };       // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  sink (vla3);
}

void access_vla3_ptr (struct S s, unsigned n)
{
  struct S vla3[3 < n ? 3 : n];
  struct S *p = &vla3[0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[3] = (struct S){ };         // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  sink (vla3);
}

void access_vla2_3_ptr (struct S s, unsigned n)
{
  struct S vla2_3[2 < n ? 2 : n][3 < n ? 3 : n];
  struct S *p = &vla2_3[0][0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[6] = (struct S){ };         // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  sink (vla2_3);
}

void access_vla3_4_5_ptr (struct S s, unsigned n)
{
  struct S vla3_4_5[3 < n ? 3 : n][4 < n ? 4 : n][5 < n ? 5 : n];
  struct S *p = &vla3_4_5[0][0][0];

  p[0] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[1] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[2] = (struct S){ };         // { dg-bogus "\\\[-Warray-bounds" }
  p[60] = (struct S){ };        // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }

  sink (vla3_4_5);
}
