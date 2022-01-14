/* PR middle-end/101475 - missing -Wstringop-overflow storing a compound
   literal
   { dg-do compile }
   { dg-options "-O2 -fno-tree-vectorize" } */

extern char ea1[1], ea2[2], ea3[3], ea4[4];

/* The trailing A member of all of Sx, S0, and S1 is treated the same:
   as a flexible array member.  */
struct Sx { char n, a[]; };
struct S0 { char n, a[0]; };
struct S1 { char n, a[1]; };
/* The trailing A member in both S2 and S3 is treated as an ordinary
   array with exactly two elements and accesses to elements beyond
   the last are diagnosed regardless of whether they are within
   the bounds the enclosing object.  */
struct S2 { char n, a[2]; };
struct S3 { char n, a[3]; };


void fx_ea1 (void)
{
  struct Sx *p = (struct Sx*)ea1;
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea1 (void)
{
  struct S0 *p = (struct S0*)ea1;
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea1 (void)
{
  struct S1 *p = (struct S1*)ea1;
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea1 (void)
{
  struct S2 *p = (struct S2*)ea1;
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea1 (void)
{
  struct S3 *p = (struct S3*)ea1;
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}


void fx_ea1_p1 (void)
{
  struct Sx *p = (struct Sx*)(ea1 + 1);
  p->n = 0;         // { dg-warning "-Wstringop-overflow" }
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea1_p1 (void)
{
  struct S0 *p = (struct S0*)(ea1 + 1);
  p->n = 0;         // { dg-warning "-Wstringop-overflow" }
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea1_p1 (void)
{
  struct S1 *p = (struct S1*)(ea1 + 1);
  p->n = 0;         // { dg-warning "-Wstringop-overflow" }
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea1_p1 (void)
{
  struct S2 *p = (struct S2*)(ea1 + 1);
  p->n = 0;         // { dg-warning "-Wstringop-overflow" }
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea1_p1 (void)
{
  struct S3 *p = (struct S3*)(ea1 + 1);
  p->n = 0;         // { dg-warning "-Wstringop-overflow" }
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}


void fx_ea2 (void)
{
  struct Sx *p = (struct Sx*)ea2;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea2 (void)
{
  struct S0 *p = (struct S0*)ea2;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea2 (void)
{
  struct S1 *p = (struct S1*)ea2;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea2 (void)
{
  struct S2 *p = (struct S2*)ea2;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea2 (void)
{
  struct S3 *p = (struct S3*)ea2;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}


void fx_ea2_p1 (void)
{
  struct Sx *p = (struct Sx*)(ea2 + 1);
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" } 
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea2_p1 (void)
{
  struct S0 *p = (struct S0*)(ea2 + 1);
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea2_p1 (void)
{
  struct S1 *p = (struct S1*)(ea2 + 1);
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea2_p1 (void)
{
  struct S2 *p = (struct S2*)(ea2 + 1);
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea2_p1 (void)
{
  struct S3 *p = (struct S3*)(ea2 + 1);
  p->n = 0;
  p->a[0] = 0;      // { dg-warning "-Wstringop-overflow" }
  p->a[1] = 1;      // { dg-warning "-Wstringop-overflow" }
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}


void fx_ea3 (void)
{
  struct Sx *p = (struct Sx*)ea3;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea3 (void)
{
  struct S0 *p = (struct S0*)ea3;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea3 (void)
{
  struct S1 *p = (struct S1*)ea3;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea3 (void)
{
  struct S2 *p = (struct S2*)ea3;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea3 (void)
{
  struct S3 *p = (struct S3*)ea3;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}


void fx_ea4 (void)
{
  struct Sx *p = (struct Sx*)ea4;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f0_ea4 (void)
{
  struct S0 *p = (struct S0*)ea4;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f1_ea4 (void)
{
  struct S1 *p = (struct S1*)ea4;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f2_ea4 (void)
{
  struct S2 *p = (struct S2*)ea4;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  /* Even though the offset of p->a[2] is within the bounds of EA4
     the warning triggers because it only considers trailing arrays
     of at mnost one element as "poor man's flexible arrays."  */
  p->a[2] = 2;      // { dg-warning "-Wstringop-overflow" }
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}

void f3_ea4 (void)
{
  struct S3 *p = (struct S3*)ea4;
  p->n = 0;
  p->a[0] = 0;
  p->a[1] = 1;
  p->a[2] = 2;
  p->a[3] = 3;      // { dg-warning "-Wstringop-overflow" }
}
