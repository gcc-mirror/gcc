/* PR tree-optimization/86914 - wrong code with strlen() of poor-man's
   flexible array member plus offset
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

struct A0 { char i, a[0]; };
struct A1 { char i, a[1]; };
struct A9 { char i, a[9]; };
struct Ax { char i, a[]; };

extern int a[];

extern struct A0 a0;
extern struct A1 a1;
extern struct A9 a9;
extern struct Ax ax;

void test_var_flexarray_cst_off (void)
{
  /* Use arbitrary constants greater than 16 in case GCC ever starts
     unrolling strlen() calls with small array arguments.  */
  a[0] = 17 < strlen (a0.a + 1);        // { dg-warning "\\\[-Warray-bounds|-Wstringop-overread" }
  a[1] = 19 < strlen (a1.a + 1);        // { dg-warning "\\\[-Wstringop-overread" }
  a[2] = 23 < strlen (a9.a + 9);        // { dg-warning "\\\[-Wstringop-overread" }
  a[3] = 29 < strlen (ax.a + 3);
}

void test_ptr_flexarray_cst_off (struct A0 *p0, struct A1 *p1,
				 struct A9 *p9, struct Ax *px)
{
  a[0] = 17 < strlen (p0->a + 1);
  a[1] = 19 < strlen (p1->a + 1);
  a[2] = 23 < strlen (p9->a + 9);
  a[3] = 29 < strlen (px->a + 3);
}

void test_ptr_flexarray_var_off (struct A0 *p0, struct A1 *p1,
				 struct A9 *p9, struct Ax *px,
				 int i)
{
  a[0] = 17 < strlen (p0->a + i);
  a[1] = 19 < strlen (p1->a + i);
  a[2] = 23 < strlen (p9->a + i);
  a[3] = 29 < strlen (px->a + i);
}

/* { dg-final { scan-tree-dump-times "strlen" 12 "optimized" } } */
