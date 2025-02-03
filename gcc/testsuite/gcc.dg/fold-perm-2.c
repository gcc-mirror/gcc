/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */
/* { dg-additional-options "--param=riscv-two-source-permutes" { target riscv*-*-* } } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));
typedef unsigned int vecu __attribute__ ((vector_size (4 * sizeof (unsigned int))));

void fun (veci *a, veci *b, veci *c)
{
  veci r1 = __builtin_shufflevector (*a, *b, 0, 5, 2, 7);
  vecu r2 = __builtin_convertvector (r1, vecu);
  vecu r3 = __builtin_shufflevector (r2, r2, 2, 3, 1, 0);
  *c = __builtin_convertvector (r3, veci);
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 2, 7, 5, 0 }" "fre1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "fre1" } } */
