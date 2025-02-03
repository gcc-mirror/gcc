/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1 -Wno-psabi" } */
/* { dg-additional-options "--param=riscv-two-source-permutes" { target riscv*-*-* } } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));

void fun (veci a, veci b, veci *i)
{
  veci c = __builtin_shuffle (a, b, __extension__ (veci) {1, 4, 2, 7});
  *i = __builtin_shuffle (c, __extension__ (veci) { 7, 2, 1, 5 });
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 3, 6, 0, 0 }" "dse1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "dse1" } } */
