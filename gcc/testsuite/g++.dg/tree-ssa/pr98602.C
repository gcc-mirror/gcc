/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O1 -fdump-tree-forwprop1-raw -Wno-psabi" } */

typedef int v4si __attribute__ ((vector_size (16)));
typedef unsigned int v4ui __attribute__ ((vector_size (16)));

v4si
smax_nonuniform (v4si x)
{
  return x >= (v4si) { -99, -98, -97, -96 }
	 ? x : (v4si) { -100, -99, -98, -97 };
}

v4si
smax_nonuniform_rev (v4si x)
{
  return x < (v4si) { -99, -98, -97, -96 }
	 ? (v4si) { -100, -99, -98, -97 } : x;
}

v4si
smin_nonuniform (v4si x)
{
  return x <= (v4si) { 99, 100, 101, 102 }
	 ? x : (v4si) { 100, 101, 102, 103 };
}

v4ui
umin_nonuniform_rev (v4ui x)
{
  return x > (v4ui) { 1, 2, 3, 4 }
	 ? (v4ui) { 2, 3, 4, 5 } : x;
}

/* { dg-final { scan-tree-dump-times "max_expr, " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "min_expr, " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "vec_cond_expr, " "forwprop1" } } */
