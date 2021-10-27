/* PR tree-optimization/35503 - Warning about restricted pointers?
   { dg-do compile }
   { dg-options "-O2 -Wno-format-overflow -Wrestrict -ftrack-macro-expansion=0" }
*/

void sink (int);

#define S10 "0123456789"

extern char a2[2][22];

#define T(d, ...) do {					\
    char a[22] = S10;					\
    sink (__builtin_sprintf ((d), __VA_ARGS__));	\
  } while (0)

void test_ptr (char *d, int i)
{
  T (d, "%s", d);       /* { dg-warning "argument 3 overlaps destination object 'd'" } */
  T (d, "%s", d + 0);   /* { dg-warning "overlaps" } */

  /* The following only overlaps if d[1] is non-zero.  */
  T (d, "%s", d + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", d + 2);   /* { dg-warning "may overlap" } */
  T (d, "%s", d + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &d[0]);   /* { dg-warning "overlaps" } */
  T (d, "%s", &d[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &d[2]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &d[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", d);   /* { dg-warning "overlaps" } */
  T (d + 1, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + i, "%s", d);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", d);   /* { dg-warning "overlaps" } */
  T (&d[1], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[i], "%s", d);   /* { dg-warning "may overlap" } */

  const char *s = d;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */
}

void test_ptr_plus (char *d, int i)
{
  const char *s = d;

  T (d, "%s", s++);     /* { dg-warning "overlaps" } */
  T (d, "%s", s++);     /* { dg-warning "may overlap" } */
  T (d, "%s", s++);     /* { dg-warning "may overlap" } */
  T (d, "%s", s++);     /* { dg-warning "may overlap" } */

  s += i;
  T (d, "%s", s);       /* { dg-warning "may overlap" } */
}

void test_array_1_dim (int i)
{
  T (a, "%s", a);       /* { dg-warning "overlaps" } */
  T (a, "%s", a + 0);   /* { dg-warning "overlaps" } */
  T (a, "%s", a + 1);   /* { dg-warning "overlaps" } */
  T (a, "%s", a + 2);   /* { dg-warning "overlaps" } */
  T (a, "%s", a + i);   /* { dg-warning "may overlap" } */

  T (a, "%s", &a[0]);   /* { dg-warning "overlaps" } */
  T (a, "%s", &a[1]);   /* { dg-warning "overlaps" } */
  T (a, "%s", &a[2]);   /* { dg-warning "overlaps" } */
  T (a, "%s", &a[i]);   /* { dg-warning "may overlap" } */

  T (a + 0, "%s", a);   /* { dg-warning "overlaps" } */
  T (a + 1, "%s", a);   /* { dg-warning "overlaps" } */
  T (a + 2, "%s", a);   /* { dg-warning "overlaps" } */
  T (a + i, "%s", a);   /* { dg-warning "may overlap" } */

  T (&a[0], "%s", a);   /* { dg-warning "overlaps" } */
  T (&a[1], "%s", a);   /* { dg-warning "overlaps" } */
  T (&a[2], "%s", a);   /* { dg-warning "overlaps" } */
  T (&a[i], "%s", a);   /* { dg-warning "may overlap" } */
}


void test_array_2_dim (int i)
{
  T (a2[0], "%s", a2[0]);       /* { dg-warning "overlaps" } */
  T (a2[0], "%s", a2[0] + 0);   /* { dg-warning "overlaps" } */
  T (a2[0], "%s", a2[0] + 1);   /* { dg-warning "may overlap" } */
  T (a2[0], "%s", a2[0] + 2);   /* { dg-warning "may overlap" } */
  T (a2[0], "%s", a2[0] + i);   /* { dg-warning "may overlap" } */

  T (a2[0], "%s", &a2[0][0]);   /* { dg-warning "overlaps" } */
  T (a2[0], "%s", &a2[0][1]);   /* { dg-warning "may overlap" } */
  T (a2[0], "%s", &a2[0][2]);   /* { dg-warning "may overlap" } */
  T (a2[0], "%s", &a2[0][i]);   /* { dg-warning "may overlap" } */

  T (a2[0] + 0, "%s", a2[0]);   /* { dg-warning "overlaps" } */
  T (a2[0] + 1, "%s", a2[0]);   /* { dg-warning "may overlap" } */
  T (a2[0] + 2, "%s", a2[0]);   /* { dg-warning "may overlap" } */
  T (a2[0] + i, "%s", a2[0]);   /* { dg-warning "may overlap" } */

  T (&a2[0][0], "%s", a2[0]);   /* { dg-warning "overlaps" } */
  T (&a2[0][1], "%s", a2[0]);   /* { dg-warning "may overlap" } */
  T (&a2[0][2], "%s", a2[0]);   /* { dg-warning "may overlap" } */
  T (&a2[0][i], "%s", a2[0]);   /* { dg-warning "may overlap" } */


  T (a2[0], "%s", a2[1]);
  T (a2[0], "%s", a2[1] + 0);
  T (a2[0], "%s", a2[1] + 1);
  T (a2[0], "%s", a2[1] + 2);
  T (a2[0], "%s", a2[1] + i);

  T (a2[0], "%s", &a2[1][0]);
  T (a2[0], "%s", &a2[1][1]);
  T (a2[0], "%s", &a2[1][2]);

  /* a2[0] is represented as &a in Gimple, and &a2[1][i] as &a + _2,
     with _1 defined to something like 10 + _1, and _1 to i.  That
     makes it virtually impossible to reliably determine that the
     two pointers refer to distinct sub-arrays of the same multi-
     dimensional array.  */
  T (a2[0], "%s", &a2[1][i]);   /* { dg-bogus "overlap" "" { xfail *-*-* } } */

  T (a2[0] + 0, "%s", a2[1]);
  T (a2[0] + 1, "%s", a2[1]);
  T (a2[0] + 2, "%s", a2[1]);
  T (a2[0] + i, "%s", a2[1]);

  T (&a2[0][0], "%s", a2[1]);
  T (&a2[0][1], "%s", a2[1]);
  T (&a2[0][2], "%s", a2[1]);
  T (&a2[0][i], "%s", a2[1]);


  T (a2[1], "%s", a2[0]);
  T (a2[1], "%s", a2[0] + 0);
  T (a2[1], "%s", a2[0] + 1);
  T (a2[1], "%s", a2[0] + 2);
  T (a2[1], "%s", a2[0] + i);

  T (a2[1], "%s", &a2[0][0]);
  T (a2[1], "%s", &a2[0][1]);
  T (a2[1], "%s", &a2[0][2]);
  T (a2[1], "%s", &a2[0][i]);

  T (a2[1] + 0, "%s", a2[0]);
  T (a2[1] + 1, "%s", a2[0]);
  T (a2[1] + 2, "%s", a2[0]);
  T (a2[1] + i, "%s", a2[0]);

  T (&a2[1][0], "%s", a2[0]);
  T (&a2[1][1], "%s", a2[0]);
  T (&a2[1][2], "%s", a2[0]);
  T (&a2[1][i], "%s", a2[0]);   /* { dg-bogus "overlap" "" { xfail *-*-* } } */


  T (a2[1], "%s", a2[1]);       /* { dg-warning "overlaps" } */
  T (a2[1], "%s", a2[1] + 0);   /* { dg-warning "overlaps" } */
  T (a2[1], "%s", a2[1] + 1);   /* { dg-warning "may overlap" } */
  T (a2[1], "%s", a2[1] + 2);   /* { dg-warning "may overlap" } */
  T (a2[1], "%s", a2[1] + i);   /* { dg-warning "may overlap" } */

  T (a2[1], "%s", &a2[1][0]);   /* { dg-warning "overlaps" } */
  T (a2[1], "%s", &a2[1][1]);   /* { dg-warning "may overlap" } */
  T (a2[1], "%s", &a2[1][2]);   /* { dg-warning "may overlap" } */
  T (a2[1], "%s", &a2[1][i]);   /* { dg-warning "may overlap" "" { xfail *-*-* } } */

  T (a2[1] + 0, "%s", a2[1]);   /* { dg-warning "overlaps" } */
  T (a2[1] + 1, "%s", a2[1]);   /* { dg-warning "may overlap" } */
  T (a2[1] + 2, "%s", a2[1]);   /* { dg-warning "may overlap" } */
  T (a2[1] + i, "%s", a2[1]);   /* { dg-warning "may overlap" } */

  T (&a2[1][0], "%s", a2[1]);   /* { dg-warning "overlaps" } */
  T (&a2[1][1], "%s", a2[1]);   /* { dg-warning "may overlap" } */
  T (&a2[1][2], "%s", a2[1]);   /* { dg-warning "may overlap" } */
  T (&a2[1][i], "%s", a2[1]);   /* { dg-warning "may overlap" "" { xfail *-*-* } } */
}

struct S {
  char a[4];
  char b[4];
};

struct S2 {
  struct S s_1;
  struct S s_2;
  struct S sa3[3];
};

struct S3 {
  struct S2 s2_1;
  struct S2 s2_2;

  struct {
    struct {
      struct {
	struct S sa_3[3];
      } a_1[3];
    } a_2[3][3];
  } a_3[3][3][3];

  char fa[];
};

void test_struct_member_array (struct S3 *s3, int i)
{
  char *d = s3->s2_1.s_1.a;

  T (d, "%s", d);       /* { dg-warning "overlaps" } */
  T (d, "%s", d + 0);   /* { dg-warning "overlaps" } */
  T (d, "%s", d + 1);   /* { dg-warning "may overlap" } */
  /* Since d below points to char[4], strlen(d + 2) must be at most 1
     and so the call cannot overlap. */
  T (d, "%s", d + 2);
  T (d, "%s", d + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &d[0]);   /* { dg-warning "overlaps" } */
  T (d, "%s", &d[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &d[2]);
  T (d, "%s", &d[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", d);   /* { dg-warning "overlaps" } */
  T (d + 1, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + i, "%s", d);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", d);   /* { dg-warning "overlaps" } */
  T (&d[1], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[i], "%s", d);   /* { dg-warning "may overlap" } */

  const char *s = d;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  s = s3->s2_1.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_1.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_1.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  d = s3->s2_1.s_1.b;

  s = s3->s2_1.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_1.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_1.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  d = s3->s2_2.s_1.a;

  s = s3->s2_1.s_1.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_1.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_1.a;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  s = s3->s2_2.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  d = s3->s2_2.s_1.b;

  s = s3->s2_1.s_1.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_1.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_1.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_1.b;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  s = s3->s2_2.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = s3->s2_2.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);
}

void test_struct_member_array_array (struct S3 *s3, int i)
{
  char *d = s3->s2_1.sa3[0].a;
  char *s = s3->s2_1.sa3[0].a;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 0);   /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &s[0]);   /* { dg-warning "overlaps" } */
  T (d, "%s", &s[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[2]);
  T (d, "%s", &s[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", s);   /* { dg-warning "overlaps" } */
  T (d + 1, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + i, "%s", s);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", s);   /* { dg-warning "overlaps" } */
  T (&d[1], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[i], "%s", s);   /* { dg-warning "may overlap" } */

  s = s3->s2_1.sa3[0].b;

  T (d, "%s", s);
  T (d, "%s", s + 0);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  T (d, "%s", &s[0]);
  T (d, "%s", &s[1]);
  T (d, "%s", &s[2]);
  T (d, "%s", &s[i]);

  T (d + 0, "%s", s);
  T (d + 1, "%s", s);
  T (d + 2, "%s", s);
  T (d + i, "%s", s);

  T (&d[0], "%s", s);
  T (&d[1], "%s", s);
  T (&d[2], "%s", s);
  T (&d[i], "%s", s);
}

void test_struct_member_nested_array (struct S3 *s3, int i)
{
#define PFX(sfx) s3->a_3[i3_0][i3_1][i3_2]	\
    .a_2[i2_0][i2_1]				\
    .a_1[i1_0].sfx

#define TT(d, s)				\
  T (PFX (d), "%s", PFX (s));			\
  T (PFX (d), "%s", &PFX (s)[0]);		\
  T (PFX (d), "%s", &PFX (s)[1]);		\
  T (PFX (d), "%s", &PFX (s)[2]);		\
  T (PFX (d), "%s", &PFX (s)[i])		\

#define T1(i0)	do						\
    {								\
      enum {i1_0 = i0 };					\
								\
      TT (sa_3[0].a, sa_3[0].b); TT (sa_3[0].b, sa_3[0].a);	\
      								\
      TT (sa_3[0].a, sa_3[1].a); TT (sa_3[0].a, sa_3[1].b);	\
      TT (sa_3[0].b, sa_3[1].a); TT (sa_3[0].b, sa_3[1].b);	\
    								\
      TT (sa_3[0].a, sa_3[2].a); TT (sa_3[0].a, sa_3[2].b);	\
      TT (sa_3[0].b, sa_3[2].a); TT (sa_3[0].b, sa_3[2].b);	\
								\
      TT (sa_3[1].a, sa_3[0].a); TT (sa_3[1].a, sa_3[0].b);	\
      TT (sa_3[1].b, sa_3[0].a); TT (sa_3[1].b, sa_3[0].b);	\
								\
      TT (sa_3[1].a, sa_3[1].b); TT (sa_3[1].b, sa_3[1].a);	\
								\
      TT (sa_3[1].a, sa_3[2].a); TT (sa_3[1].a, sa_3[2].b);	\
      TT (sa_3[1].b, sa_3[2].a); TT (sa_3[1].b, sa_3[2].b);	\
    } while (0)

#define T2(i0, i1) do				\
    {						\
      enum { i2_0 = i0, i2_1 = i1 };		\
      T1 (0); T1 (1); T1 (2);			\
    } while (0)

#define T3(i0, i1, i2) do			\
    {						\
      enum { i3_0 = i0, i3_1 = i1, i3_2 = i2 };	\
      T2 (0, 0); T2 (0, 1); T2 (0, 2);		\
      T2 (1, 0); T2 (1, 1); T2 (1, 2);		\
      T2 (2, 0); T2 (2, 1); T2 (2, 2);		\
    } while (0)

#if 0
  /* These tests take forever and a day to compile.  Enable them only
     during the development of this feature but leave them otherwise
     disabled to avoid slowing everything down for others.  */
  T3 (0, 0, 0); T3 (0, 0, 1); T3 (0, 0, 2);
  T3 (0, 1, 0); T3 (0, 1, 1); T3 (0, 1, 2);
  T3 (0, 2, 0); T3 (0, 2, 1); T3 (0, 2, 2);

  T3 (1, 0, 0); T3 (1, 0, 1); T3 (1, 0, 2);
  T3 (1, 1, 0); T3 (1, 1, 1); T3 (1, 1, 2);
  T3 (1, 2, 0); T3 (1, 2, 1); T3 (1, 2, 2);

  T3 (2, 0, 0); T3 (2, 0, 1); T3 (2, 0, 2);
  T3 (2, 1, 0); T3 (2, 1, 1); T3 (2, 1, 2);
  T3 (2, 2, 0); T3 (2, 2, 1); T3 (2, 2, 2);
#endif
}

void test_struct_member_flexarray (struct S3 *s3, int i, int j)
{
  char *d = s3->fa;
  char *s = s3->fa;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 0);   /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &s[0]);   /* { dg-warning "overlaps" } */
  T (d, "%s", &s[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[2]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", s);   /* { dg-warning "overlaps" } */
  T (d + 1, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + i, "%s", s);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", s);   /* { dg-warning "overlaps" } */
  T (&d[1], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[i], "%s", s);   /* { dg-warning "may overlap" } */

  d = s3->fa + i;
  s = s3->fa + j;

  T (d, "%s", s);       /* { dg-warning "may overlap" } */
  T (d, "%s", s + 0);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &s[0]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[2]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &s[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + 1, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", s);   /* { dg-warning "may overlap" } */
  T (d + j, "%s", s);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[1], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", s);   /* { dg-warning "may overlap" } */
  T (&d[j], "%s", s);   /* { dg-warning "may overlap" } */
}

union U
{
  struct S2 s2_1;
  struct S2 s2_2;
};

void test_union_member_array (union U *un, int i)
{
  char *d = un->s2_1.s_1.a;

  T (d, "%s", d);       /* { dg-warning "overlaps" } */
  T (d, "%s", d + 0);   /* { dg-warning "overlaps" } */
  T (d, "%s", d + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", d + 2);
  T (d, "%s", d + i);   /* { dg-warning "may overlap" } */

  T (d, "%s", &d[0]);   /* { dg-warning "overlaps" } */
  T (d, "%s", &d[1]);   /* { dg-warning "may overlap" } */
  T (d, "%s", &d[2]);
  T (d, "%s", &d[i]);   /* { dg-warning "may overlap" } */

  T (d + 0, "%s", d);   /* { dg-warning "overlaps" } */
  T (d + 1, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + 2, "%s", d);   /* { dg-warning "may overlap" } */
  T (d + i, "%s", d);   /* { dg-warning "may overlap" } */

  T (&d[0], "%s", d);   /* { dg-warning "overlaps" } */
  T (&d[1], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[2], "%s", d);   /* { dg-warning "may overlap" } */
  T (&d[i], "%s", d);   /* { dg-warning "may overlap" } */

  const char *s = d;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  s = un->s2_1.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = un->s2_1.s_2.a;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = un->s2_1.s_2.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);

  s = un->s2_2.s_1.a;

  T (d, "%s", s);       /* { dg-warning "overlaps" } */
  T (d, "%s", s + 1);   /* { dg-warning "may overlap" } */
  T (d, "%s", s + 2);
  T (d, "%s", s + i);   /* { dg-warning "may overlap" } */

  s = un->s2_2.s_1.b;

  T (d, "%s", s);
  T (d, "%s", s + 1);
  T (d, "%s", s + 2);
  T (d, "%s", s + i);
}

void test_multiple_overlap (int i)
{
  {
    char a[3] = "";           /* { dg-message "declared here" } */

    /* Both a3 and a4 definitely overlap the output even if the are
       empty because of the terminating nul.  */
    char *d = a;
    char *a3 = a + 0;
    char *a4 = a - 0;

    T (d, "%s%s", a3, a4);    /* { dg-warning "arguments 3, 4 overlap destination object .a." }*/
  }

  {
    char a[4];

    /* There is no overlap here because the length of a3 is at most 1
       and a4 is necessarily the empty string.  */
    char *d = a;
    char *a3 = a + 2;
    char *a4 = a + 3;

    T (d, "%s%s", a3, a4);
  }

  {
    char a[5];                /* { dg-message "declared here" } */

    /* a3 and a4 may overlap the output.  They will only not overlap
       it when a3 is empty, and a4 is at most 1 character long.  */
    char *d = a;
    char *a3 = a + 2;
    char *a4 = a + 3;

    T (d, "%s%s", a3, a4);    /* { dg-warning "arguments 3, 4 may overlap destination object .a." }*/
  }

  {
    char a[5];                /* { dg-message "declared here" } */

    /* a3 certaibly overlaps the output, but a4 may or may not depending
       in the value of i.  */
    char *d = a;
    char *a3 = a + 0;
    char *a4 = a + i;

    T (d, "%s%s", a3, a4);    /* { dg-warning "arguments 3 and maybe 4 overlap destination object .a." }*/
  }
}

void test_overlap_with_precision (char *d, int i, int j)
{
  /* None of the following is diagnosed because no copying takes place
     between the %s argument and the destination.  */
  T (d, "%.0s", d + 0);
  T (d, "%.0s", d + 1);
  T (d, "%.0s", d + 2);
  T (d, "%.0s", d + i);

  T (d, "%.1s", d + 0);       /* { dg-warning "overlaps" } */
  /* Unlike the %.0s case the following deserves a warning because
     when d[1] isn't nul, it will be overwritten by the terminating
     nul added by sprintf.  */
  T (d, "%.1s", d + 1);       /* { dg-warning "may overlap" } */
  T (d, "%.1s", d + 2);
  T (d, "%.1s", d + i);       /* { dg-warning "may overlap" } */

  T (d + 1, "%.0s", d);
  T (d + 2, "%.0s", d);

  T (d + 1, "%.1s", d);       /* { dg-warning "may overlap" } */
  T (d + 2, "%.1s", d);

  T (d + 2, "%.1s", d + 1);   /* { dg-warning "may overlap" } */
  T (d + 2, "%.1s", d + i);   /* { dg-warning "may overlap" } */

  /* The following should be "overlaps" but tracking that the offset
     is the same variable doesn't seem worth the effort.  */
  T (d + i, "%.1s", d + i);   /* { dg-warning "overlap" } */

  T (d + i, "%.1s", d + j);   /* { dg-warning "may overlap" } */

  /* Exercise overlap with precison in the range [0, 1].  */
  if (i < 0 || 1 < i)
    i = 0;

  T (d, "%.*s", i, d + 0);    /* { dg-warning "may overlap" } */
  T (d, "%.*s", i, d + 1);    /* { dg-warning "may overlap" } */
  T (d, "%.*s", i, d + 2);
  T (d, "%.*s", i, d + i);    /* { dg-warning "may overlap" "" { xfail *-*-* } } */
}
