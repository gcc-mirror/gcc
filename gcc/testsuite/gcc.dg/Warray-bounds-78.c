/* PR tree-optimization/99475 - bogus -Warray-bounds accessing an array
   element of empty structs
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-strict-aliasing" } */

#define NOIPA __attribute__ ((noipa))

struct S { };

extern struct S sa3[3];
extern struct S sa2_3[2][3];
extern struct S sa3_4_5[3][4][5];

void sink (void*);


NOIPA void access_sa3 (void)
{
  ((bool*)sa3)[0] = __LINE__;     // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)sa3)[1] = __LINE__;     // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)sa3)[2] = __LINE__;     // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)sa3)[3] = __LINE__;     // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void access_sa3_ptr (void)
{
  bool *p = (bool*)&sa3[0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[3] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void access_sa2_3_ptr (void)
{
  bool *p = (bool*)&sa2_3[0][0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[6] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void access_sa3_4_5_ptr (struct S s, int i)
{
  bool *p = (bool*)&sa3_4_5[0][0][0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[60] = __LINE__;       // { dg-warning "\\\[-Warray-bounds" }
}


NOIPA void access_vla3 (struct S s, unsigned n)
{
  struct S vla3[3 < n ? 3 : n];

  ((bool*)vla3)[0] = __LINE__;    // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)vla3)[1] = __LINE__;    // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)vla3)[2] = __LINE__;    // { dg-warning "\\\[-Warray-bounds" }
  ((bool*)vla3)[3] = __LINE__;    // { dg-warning "\\\[-Warray-bounds" }

  sink (vla3);
}

NOIPA void access_vla3_ptr (struct S s, unsigned n)
{
  struct S vla3[3 < n ? 3 : n];
  bool *p = (bool*)&vla3[0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[3] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }

  sink (vla3);
}

NOIPA void access_vla2_3_ptr (struct S s, unsigned n)
{
  struct S vla2_3[2 < n ? 2 : n][3 < n ? 3 : n];
  bool *p = (bool*)&vla2_3[0][0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[6] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }

  sink (vla2_3);
}

NOIPA void access_vla3_4_5_ptr (struct S s, unsigned n)
{
  struct S vla3_4_5[3 < n ? 3 : n][4 < n ? 4 : n][5 < n ? 5 : n];
  bool *p = (bool*)&vla3_4_5[0][0][0];

  p[0] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[1] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[2] = __LINE__;        // { dg-warning "\\\[-Warray-bounds" }
  p[60] = __LINE__;       // { dg-warning "\\\[-Warray-bounds" }

  sink (vla3_4_5);
}

// { dg-prune-output "empty struct has size 0 in C" }
