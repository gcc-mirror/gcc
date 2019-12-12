/* PR tree-optimization/84526 - ICE in generic_overlap
   Unrelated to the ICE but rather to PR 84095 that introduced it, verify
   that calls to strncpy involving multidimensional arrays of structs don't
   trigger false positive -Wrestrict warnings.
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" }
   { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef __SIZE_TYPE__ size_t;

extern char* strcpy (char*, const char*);

struct MemArrays
{
  char a1[4];
  char a2[4][4];
  char a3[4][4][4];
} ma1[4], ma2[4][4], ma3[4][4][4];

#define T(dst, src) do {				\
    strcpy (src, "123");				\
    strcpy (dst, src);					\
  } while (0)


void test_ma1_cst (const char *s)
{
  T (ma1[0].a1, ma1[0].a1);           /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma1[0].a1, ma1[1].a1);
  T (ma1[0].a1, ma1[2].a1);
  T (ma1[0].a1, ma1[3].a1);

  T (ma1[0].a1, ma1[0].a1);           /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma1[1].a1, ma1[0].a1);
  T (ma1[2].a1, ma1[0].a1);
  T (ma1[3].a1, ma1[0].a1);
}


void test_ma1_var_cst (const char *s, int i)
{
  T (ma1[i].a1, ma1[0].a1);
  T (ma1[i].a1, ma1[1].a1);
  T (ma1[i].a1, ma1[2].a1);
  T (ma1[i].a1, ma1[3].a1);

  T (ma1[0].a1, ma1[i].a1);
  T (ma1[1].a1, ma1[i].a1);
  T (ma1[2].a1, ma1[i].a1);
  T (ma1[3].a1, ma1[i].a1);
}


void test_ma1_var_var (const char *s, int i, int j)
{
  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);

  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);
  T (ma1[i].a1, ma1[j].a1);
}


void test_ma2_cst (const char *s)
{
  T (ma2[0][0].a1, ma2[0][0].a1);     /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma2[0][0].a1, ma2[0][1].a1);
  T (ma2[0][0].a1, ma2[0][2].a1);
  T (ma2[0][0].a1, ma2[0][3].a1);

  T (ma2[0][0].a1, ma2[1][0].a1);
  T (ma2[0][0].a1, ma2[1][1].a1);
  T (ma2[0][0].a1, ma2[1][2].a1);
  T (ma2[0][0].a1, ma2[1][3].a1);

  T (ma2[0][0].a1, ma2[2][0].a1);
  T (ma2[0][0].a1, ma2[2][1].a1);
  T (ma2[0][0].a1, ma2[2][2].a1);
  T (ma2[0][0].a1, ma2[2][3].a1);

  T (ma2[0][0].a1, ma2[3][0].a1);
  T (ma2[0][0].a1, ma2[3][1].a1);
  T (ma2[0][0].a1, ma2[3][2].a1);
  T (ma2[0][0].a1, ma2[3][3].a1);


  T (ma2[0][1].a1, ma2[0][0].a1);
  T (ma2[0][1].a1, ma2[0][1].a1);     /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma2[0][1].a1, ma2[0][2].a1);
  T (ma2[0][1].a1, ma2[0][3].a1);

  T (ma2[0][1].a1, ma2[1][0].a1);
  T (ma2[0][1].a1, ma2[1][1].a1);
  T (ma2[0][1].a1, ma2[1][2].a1);
  T (ma2[0][1].a1, ma2[1][3].a1);

  T (ma2[0][1].a1, ma2[2][0].a1);
  T (ma2[0][1].a1, ma2[2][1].a1);
  T (ma2[0][1].a1, ma2[2][2].a1);
  T (ma2[0][1].a1, ma2[2][3].a1);

  T (ma2[0][1].a1, ma2[3][0].a1);
  T (ma2[0][1].a1, ma2[3][1].a1);
  T (ma2[0][1].a1, ma2[3][2].a1);
  T (ma2[0][1].a1, ma2[3][3].a1);


  T (ma2[0][2].a1, ma2[0][0].a1);
  T (ma2[0][2].a1, ma2[0][1].a1);
  T (ma2[0][2].a1, ma2[0][2].a1);     /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma2[0][2].a1, ma2[0][3].a1);

  T (ma2[0][2].a1, ma2[1][0].a1);
  T (ma2[0][2].a1, ma2[1][1].a1);
  T (ma2[0][2].a1, ma2[1][2].a1);
  T (ma2[0][2].a1, ma2[1][3].a1);

  T (ma2[0][2].a1, ma2[2][0].a1);
  T (ma2[0][2].a1, ma2[2][1].a1);
  T (ma2[0][2].a1, ma2[2][2].a1);
  T (ma2[0][2].a1, ma2[2][3].a1);

  T (ma2[0][2].a1, ma2[3][0].a1);
  T (ma2[0][2].a1, ma2[3][1].a1);
  T (ma2[0][2].a1, ma2[3][2].a1);
  T (ma2[0][2].a1, ma2[3][3].a1);


  T (ma2[0][3].a1, ma2[0][0].a1);
  T (ma2[0][3].a1, ma2[0][1].a1);
  T (ma2[0][3].a1, ma2[0][2].a1);
  T (ma2[0][3].a1, ma2[0][3].a1);     /* { dg-warning "\\\[-Wrestrict]" } */

  T (ma2[0][3].a1, ma2[1][0].a1);
  T (ma2[0][3].a1, ma2[1][1].a1);
  T (ma2[0][3].a1, ma2[1][2].a1);
  T (ma2[0][3].a1, ma2[1][3].a1);

  T (ma2[0][3].a1, ma2[2][0].a1);
  T (ma2[0][3].a1, ma2[2][1].a1);
  T (ma2[0][3].a1, ma2[2][2].a1);
  T (ma2[0][3].a1, ma2[2][3].a1);

  T (ma2[0][3].a1, ma2[3][0].a1);
  T (ma2[0][3].a1, ma2[3][1].a1);
  T (ma2[0][3].a1, ma2[3][2].a1);
  T (ma2[0][3].a1, ma2[3][3].a1);
}


void test_ma2_var (int i0, int j0, int i1, int j1)
{
  T (ma2[i0][j0].a1, ma2[i0][j0].a1);       /* { dg-warning "\\\[-Wrestrict]" } */

  T (ma2[i0][j0].a1, ma2[i0][j1].a1);       /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma2[i0][j0].a1, ma2[i1][j1].a1);       /* { dg-bogus "\\\[-Wrestrict]" } */

  T (ma2[0][0].a2[i0], ma2[0][0].a2[j0]);   /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma2[0][i0].a2[0], ma2[0][i1].a2[0]);   /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma2[i0][0].a2[0], ma2[i1][0].a2[0]);   /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma2[i0][j0].a2[0], ma2[i1][j1].a2[0]); /* { dg-bogus "\\\[-Wrestrict]" } */
}


void test_p2_var (struct MemArrays **p2, int i0, int j0, int i1, int j1)
{
  T (p2[i0][j0].a1, p2[i0][j0].a1);         /* { dg-warning "\\\[-Wrestrict]" } */

  T (p2[i0][j0].a1, p2[i0][j1].a1);
  T (p2[i0][j0].a1, p2[i1][j1].a1);

  T (p2[0][0].a2[i0], p2[0][0].a2[j0]);
  T (p2[0][i0].a2[0], p2[0][i1].a2[0]);
  T (p2[i0][0].a2[0], p2[i1][0].a2[0]);
  T (p2[i0][j0].a2[0], p2[i1][j1].a2[0]);
}


void test_ma3_cst (const char *s)
{
  T (ma3[0][0][0].a1, ma3[0][0][0].a1); /* { dg-warning "\\\[-Wrestrict]" } */
  T (ma3[0][0][0].a1, ma3[0][0][3].a1);

  T (ma3[0][0][0].a1, ma3[0][1][0].a1);
  T (ma3[0][0][0].a1, ma3[0][1][3].a1);
  T (ma3[0][0][0].a1, ma3[1][0][0].a1);
  T (ma3[0][0][0].a1, ma3[1][0][3].a1);
  T (ma3[0][0][0].a1, ma3[3][0][3].a1);
  T (ma3[0][0][0].a1, ma3[3][3][3].a1);
}


void test_ma3_var (const char *s,
		   int i0, int j0, int k0,
		   int i1, int j1, int k1)
{
  T (ma3[i0][j0][k0].a1, ma3[i0][j0][k0].a1);   /* { dg-warning "\\\[-Wrestrict]" } */

  T (ma3[i0][j0][k0].a1, ma3[i0][j0][k1].a1);   /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma3[i0][j0][k0].a1, ma3[i0][j1][k1].a1);   /* { dg-bogus "\\\[-Wrestrict]" } */
  T (ma3[i0][j0][k0].a1, ma3[i1][j1][k1].a1);   /* { dg-bogus "\\\[-Wrestrict]" } */
}
