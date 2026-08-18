/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

/* Rounding up by adding the padding, spelled with vectors.  */

typedef unsigned int v4ui __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));

v4ui f1 (v4ui x) { return x + ((-x) & 15); }
v4si f2 (v4si x) { return x + ((-x) & 63); }

/* { dg-final { scan-tree-dump-not "= -" "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\+ " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 2 "optimized" } } */
