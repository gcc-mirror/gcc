/* Verify that sprintf calls with arrays or struct of arrays don't
   cause -Wrestrict false positives.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wrestrict -ftrack-macro-expansion=0" } */

#define sprintf(d, f, ...) (sprintf (d, f, __VA_ARGS__), sink (d))

extern void sink (void*, ...);
extern int (sprintf) (char*, const char*, ...);

extern char ca[][2][8];

void test_array_of_arrays (void)
{
  sprintf (ca[0][0], "%s", ca[0][0]);     // { dg-warning "-Wrestrict" }
  sprintf (ca[0][0], "%s", ca[0][1]);
  sprintf (ca[0][0], "%s", ca[1][0]);
  sprintf (ca[0][0], "%s", ca[1][1]);

  sprintf (ca[0][1], "%s", ca[0][0]);
  sprintf (ca[0][1], "%s", ca[0][1]);     // { dg-warning "-Wrestrict" }
  sprintf (ca[0][1], "%s", ca[1][0]);
  sprintf (ca[0][1], "%s", ca[1][1]);

  sprintf (ca[1][0], "%s", ca[0][0]);
  sprintf (ca[1][0], "%s", ca[0][1]);
  sprintf (ca[1][0], "%s", ca[1][0]);     // { dg-warning "-Wrestrict" }
  sprintf (ca[1][0], "%s", ca[1][1]);

  sprintf (ca[1][1], "%s", ca[0][0]);
  sprintf (ca[1][1], "%s", ca[0][1]);
  sprintf (ca[1][1], "%s", ca[1][0]);
  sprintf (ca[1][1], "%s", ca[1][1]);     // { dg-warning "-Wrestrict" }
}


struct A
{
  char a[2][2][8];
  char b[2][2][8];
  char c[2][2][8];
};

extern struct A aa[][2];

void test_array_of_structs (void)
{
  // Verify that calls with the same elements of the same array trigger
  // warnings as expected.
  sprintf (aa[0][0].a[0][0], "%s", aa[0][0].a[0][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][0].a[0][1], "%s", aa[0][0].a[0][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][0].a[1][0], "%s", aa[0][0].a[1][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][0].a[1][1], "%s", aa[0][0].a[1][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][1].a[0][0], "%s", aa[0][1].a[0][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][1].a[0][1], "%s", aa[0][1].a[0][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][1].a[1][0], "%s", aa[0][1].a[1][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[0][1].a[1][1], "%s", aa[0][1].a[1][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][0].a[0][0], "%s", aa[1][0].a[0][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][0].a[0][1], "%s", aa[1][0].a[0][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][0].a[1][0], "%s", aa[1][0].a[1][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][0].a[1][1], "%s", aa[1][0].a[1][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][1].a[0][0], "%s", aa[1][1].a[0][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][1].a[0][1], "%s", aa[1][1].a[0][1]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][1].a[1][0], "%s", aa[1][1].a[1][0]);     // { dg-warning "-Wrestrict" }
  sprintf (aa[1][1].a[1][1], "%s", aa[1][1].a[1][1]);     // { dg-warning "-Wrestrict" }

#define NOWARN()

  // Exhaustively verify that calls with different elements of the same
  // array don't cause false positives.
#undef NOWARN
#define NOWARN(D, S)				\
  sprintf (D[0][0], "%s", S[0][0]);		\
  sprintf (D[0][0], "%s", S[0][1]);		\
  sprintf (D[0][0], "%s", S[1][0]);		\
  sprintf (D[0][0], "%s", S[1][1]);		\
  sprintf (D[0][1], "%s", S[0][0]);		\
  sprintf (D[0][1], "%s", S[0][1]);		\
  sprintf (D[0][1], "%s", S[1][0]);		\
  sprintf (D[0][1], "%s", S[1][1]);		\
  sprintf (D[1][0], "%s", S[0][0]);		\
  sprintf (D[1][0], "%s", S[0][1]);		\
  sprintf (D[1][0], "%s", S[1][0]);		\
  sprintf (D[1][0], "%s", S[1][1]);		\
  sprintf (D[1][1], "%s", S[0][0]);		\
  sprintf (D[1][1], "%s", S[0][1]);		\
  sprintf (D[1][1], "%s", S[1][0]);		\
  sprintf (D[1][1], "%s", S[1][1])

  NOWARN (aa[0][0].a, aa[0][1].a);
  NOWARN (aa[0][0].a, aa[1][0].a);
  NOWARN (aa[0][0].a, aa[1][1].a);

  NOWARN (aa[0][1].a, aa[0][0].a);
  NOWARN (aa[0][1].a, aa[1][0].a);
  NOWARN (aa[0][1].a, aa[1][1].a);

  NOWARN (aa[1][0].a, aa[0][0].a);
  NOWARN (aa[1][0].a, aa[0][1].a);
  NOWARN (aa[1][0].a, aa[1][1].a);

#define NOWARN_MEM(M1, M2)			\
  NOWARN (aa[0][0].M1, aa[0][0].M2);		\
  NOWARN (aa[0][0].M1, aa[0][1].M2);		\
  NOWARN (aa[0][0].M1, aa[1][0].M2);		\
  NOWARN (aa[0][0].M1, aa[1][1].M2)

  NOWARN_MEM (a, b);
  NOWARN_MEM (a, c);
  NOWARN_MEM (b, a);
  NOWARN_MEM (b, c);
  NOWARN_MEM (c, a);
  NOWARN_MEM (c, b);
}
