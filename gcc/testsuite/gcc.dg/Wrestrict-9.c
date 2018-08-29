/* PR tree-optimization/84095 - false-positive -Wrestrict warnings for
   strcpy within array
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void* restrict, const void* restrict, size_t);
extern char* strcpy (char* restrict, const char* restrict);

#define T(d, s)   strcpy (d, s)

struct MemArrays {
  char pad[8];
  char a2[3][9];
  char a3[3][4][9];
} a[2];

struct NestedMemArrays {
  struct MemArrays ma;
  struct MemArrays ma1[2];
} nma[2];

/* Use a variable as the source of a copy to verify that VAR_DECL
   is handled correctly when it's the source of GIMPLE assignment.  */
const char *str = "1234567";

void test_obj_2_dim_const (void)
{
  const char *s = strcpy (a[0].a2[0], "1234567");

  T (a[0].a2[1], s);
  T (a[0].a2[2], s);

  /* Ideally, the offsets in the warning below would be relative to
     the beginning of the accessed array member.  Unfortunately, when
     the offset is represented as
     MEM_REF (char[9], A, offsetof (struct MemArrays, A[0].A2[0]) + 1)
     as it is in this instance it's difficult to determine the member
     that is being accessed and tease out from the MEM_REF the offset
     as it appears in the source.  As a result, the warning mentions
     the offset from the beginning of A instead.  This is suboptimal
     and should be fixed, either by printing the correct offsets or
     by mentioning the base object that the offset is relative to.  */
  T (a[0].a2[0] + 1, s);      /* { dg-warning "accessing 8 bytes at offsets \(1|9\) and \(0|8\) overlaps 7 bytes at offset \(1|9\)." } */
  T (a[0].a2[1] + 2, s);
  T (a[0].a2[2] + 3, s);

  T (a[1].a2[0], s);
  T (a[1].a2[1], s);
  T (a[1].a2[2], s);

  T (a[1].a2[0] + 1, s);
  T (a[1].a2[1] + 2, s);
  T (a[1].a2[2] + 3, s);
}

void test_obj_nested_2_dim_const (void)
{
  const char *s = strcpy (nma[0].ma.a2[0], str);

  T (nma[0].ma.a2[1], s);
  T (nma[0].ma.a2[2], s);

  T (nma[0].ma.a2[0] + 1, s);      /* { dg-warning "accessing 8 bytes at offsets 1 and 0 overlaps 7 bytes at offset 1" "bug " { xfail *-*-* } } */
  T (nma[0].ma.a2[1] + 2, s);
  T (nma[0].ma.a2[2] + 3, s);

  T (nma[1].ma.a2[1], s);
  T (nma[1].ma.a2[2], s);

  T (nma[1].ma.a2[0] + 1, s);
  T (nma[1].ma.a2[1] + 2, s);
  T (nma[1].ma.a2[2] + 3, s);


  T (nma[0].ma1[0].a2[1], s);
  T (nma[0].ma1[0].a2[2], s);

  T (nma[0].ma1[0].a2[0] + 1, s);
  T (nma[0].ma1[0].a2[1] + 2, s);
  T (nma[0].ma1[0].a2[2] + 3, s);

  T (nma[1].ma1[0].a2[1], s);
  T (nma[1].ma1[0].a2[2], s);

  T (nma[1].ma1[0].a2[0] + 1, s);
  T (nma[1].ma1[0].a2[1] + 2, s);
  T (nma[1].ma1[0].a2[2] + 3, s);
}

void test_obj_2_dim_var (int i, int j)
{
  const char *s = memcpy (a[0].a2[0], "1234567", 8);

  T (a[i].a2[0], s);          /* { dg-bogus "\\\[-Wrestrict]" } */
  T (a[i].a2[1], s);
  T (a[i].a2[2], s);

  T (a[i].a2[0] + 1, s);
  T (a[i].a2[1] + 1, s);
  T (a[i].a2[2] + 1, s);

  T (a[0].a2[i], s);          /* { dg-bogus "\\\[-Wrestrict]" } */
  T (a[1].a2[i], s);

  T (a[i].a2[0] + j, s);
  T (a[i].a2[1] + j, s);
  T (a[i].a2[2] + j, s);

  T (a[0].a2[i] + 1, s);
  T (a[1].a2[i] + 1, s);

  T (a[0].a2[i] + j, s);
  T (a[1].a2[i] + j, s);

  if (i < 0 || 1 < i)
    i = 1;

  T (a[i].a2[0], s);
  T (a[i].a2[1], s);
  T (a[i].a2[2], s);

  T (a[i].a2[0] + 1, s);
  T (a[i].a2[1] + 1, s);
  T (a[i].a2[2] + 1, s);

  T (a[0].a2[i], s);
  T (a[1].a2[i], s);

  T (a[i].a2[0] + j, s);
  T (a[i].a2[1] + j, s);
  T (a[i].a2[2] + j, s);

  T (a[0].a2[i] + 1, s);
  T (a[1].a2[i] + 1, s);

  T (a[0].a2[i] + j, s);
  T (a[1].a2[i] + j, s);
}

void test_obj_nested_2_dim_var (int i, int j)
{
  const char *s = strcpy (nma[0].ma.a2[0], "1234567");

  T (nma[i].ma.a2[0], s);     /* { dg-bogus "\\\[-Wrestrict]" } */
  T (nma[i].ma.a2[1], s);
  T (nma[i].ma.a2[2], s);

  T (nma[i].ma.a2[0] + 1, s);
  T (nma[i].ma.a2[1] + 1, s);
  T (nma[i].ma.a2[2] + 1, s);

  T (nma[0].ma.a2[i], s);     /* { dg-bogus "\\\[-Wrestrict]" } */
  T (nma[1].ma.a2[i], s);

  T (nma[i].ma.a2[0] + j, s);
  T (nma[i].ma.a2[1] + j, s);
  T (nma[i].ma.a2[2] + j, s);

  T (nma[0].ma.a2[i] + 1, s);
  T (nma[1].ma.a2[i] + 1, s);

  T (nma[0].ma.a2[i] + j, s);
  T (nma[1].ma.a2[i] + j, s);
}

void test_ref_2_dim_const (struct MemArrays *p)
{
  strcpy (p[0].a2[0], "1234567");
  const char *s = p[0].a2[0];

  T (p[0].a2[1], s);
  T (p[0].a2[2], s);

  T (p[1].a2[0], s);
  T (p[1].a2[1], s);
  T (p[1].a2[2], s);
}

void test_ref_2_dim_var (struct MemArrays *p, int i, int j)
{
  strcpy (p[0].a2[0], "1234567");
  const char *s = p[0].a2[0];

  T (p[i].a2[0], s);          /* { dg-bogus "\\\[-Wrestrict]" } */
  T (p[i].a2[1], s);
  T (p[i].a2[2], s);

  T (p[0].a2[i], s);
  T (p[1].a2[i], s);

  T (p[i].a2[0] + j, s);
  T (p[i].a2[1] + j, s);
  T (p[i].a2[2] + j, s);

  T (p[0].a2[i] + j, s);
  T (p[1].a2[i] + j, s);
}

void test_obj_3_dim_var (int i, int j)
{
  strcpy (a[0].a3[0][0], "1234567");
  const char *s = a[0].a3[0][0];

  T (a[0].a3[0][i], s);
  T (a[0].a3[1][i], s);
  T (a[0].a3[2][i], s);

  T (a[1].a3[0][i], s);
  T (a[1].a3[1][i], s);
  T (a[1].a3[2][i], s);

  T (a[0].a3[i][0], s);       /* { dg-bogus "\\\[-Wrestrict\]" } */
  T (a[0].a3[i][1], s);
  T (a[0].a3[i][2], s);

  T (a[1].a3[i][0], s);
  T (a[1].a3[i][1], s);
  T (a[1].a3[i][2], s);

  T (a[i].a3[0][0], s);       /* { dg-bogus "\\\[-Wrestrict\]" } */
  T (a[i].a3[0][1], s);
  T (a[i].a3[0][2], s);

  T (a[i].a3[1][0], s);
  T (a[i].a3[1][1], s);
  T (a[i].a3[1][2], s);

  T (a[i].a3[2][0], s);
  T (a[i].a3[2][1], s);
  T (a[i].a3[2][2], s);


  T (a[0].a3[0][i] + 1, s);
  T (a[0].a3[1][i] + 1, s);
  T (a[0].a3[2][i] + 1, s);

  T (a[1].a3[0][i] + 1, s);
  T (a[1].a3[1][i] + 1, s);
  T (a[1].a3[2][i] + 1, s);


  T (a[0].a3[0][i] + j, s);
  T (a[0].a3[1][i] + j, s);
  T (a[0].a3[2][i] + j, s);

  T (a[1].a3[0][i] + j, s);
  T (a[1].a3[1][i] + j, s);
  T (a[1].a3[2][i] + j, s);

  T (a[0].a3[i][0] + j, s);
  T (a[0].a3[i][1] + j, s);
  T (a[0].a3[i][2] + j, s);

  T (a[1].a3[i][0] + j, s);
  T (a[1].a3[i][1] + j, s);
  T (a[1].a3[i][2] + j, s);

  T (a[i].a3[0][0] + j, s);
  T (a[i].a3[0][1] + j, s);
  T (a[i].a3[0][2] + j, s);

  T (a[i].a3[1][0] + j, s);
  T (a[i].a3[1][1] + j, s);
  T (a[i].a3[1][2] + j, s);

  T (a[i].a3[2][0] + j, s);
  T (a[i].a3[2][1] + j, s);
  T (a[i].a3[2][2] + j, s);
}

void test_obj_3_dim_const (struct MemArrays *p)
{
  strcpy (p[0].a3[0][0], "1234567");
  const char *s = p[0].a3[0][0];

  T (p[0].a3[0][1], s);
  T (p[0].a3[0][2], s);
  T (p[0].a3[0][3], s);

  T (p[0].a3[0][1] + 1, s);
  T (p[0].a3[0][2] + 1, s);
  T (p[0].a3[0][3] + 1, s);

  T (p[0].a3[1][0], s);
  T (p[0].a3[1][1], s);
  T (p[0].a3[1][2], s);
  T (p[0].a3[1][3], s);

  T (p[0].a3[1][0] + 1, s);
  T (p[0].a3[1][1] + 1, s);
  T (p[0].a3[1][2] + 1, s);
  T (p[0].a3[1][3] + 1, s);

  T (p[0].a3[2][0], s);
  T (p[0].a3[2][1], s);
  T (p[0].a3[2][2], s);
  T (p[0].a3[2][3], s);

  T (p[1].a3[0][0], s);
  T (p[1].a3[0][1], s);
  T (p[1].a3[0][2], s);
  T (p[1].a3[0][3], s);

  T (p[1].a3[1][0], s);
  T (p[1].a3[1][1], s);
  T (p[1].a3[1][2], s);
  T (p[1].a3[1][3], s);

  T (p[1].a3[2][0], s);
  T (p[1].a3[2][1], s);
  T (p[1].a3[2][2], s);
  T (p[1].a3[2][3], s);
}
