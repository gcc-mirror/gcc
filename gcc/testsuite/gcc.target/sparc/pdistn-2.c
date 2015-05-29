/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis3 -O1 -fdump-tree-optimized" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));

#define _(A) (unsigned char)A

long foo () {
  vec8 a = { _(1), _(2), _(3), _(4), _(5), _(6), _(7), _(255) };
  vec8 b = { _(2), _(4), _(8), _(16), _(32), _(64), _(128), _(8) };
  return __builtin_vis_pdistn (a, b);
}

/* { dg-final { scan-assembler-not "pdistn\t%" } } */
/* { dg-final { scan-tree-dump "return 473" "optimized" } } */
