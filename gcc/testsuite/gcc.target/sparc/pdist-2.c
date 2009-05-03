/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis -O1 -fdump-tree-optimized" } */

typedef long long int64_t;
typedef unsigned char vec8 __attribute__((vector_size(8)));

#define _(A) (unsigned char)A

int64_t foo () {
  int64_t d = 2;
  vec8 a = { _(1), _(2), _(3), _(4), _(5), _(6), _(7), _(255) };
  vec8 b = { _(2), _(4), _(8), _(16), _(32), _(64), _(128), _(8) };
  d = __builtin_vis_pdist (a, b, d);
  return d;
}

/* { dg-final { scan-assembler-not "pdist\t%" } } */
/* { dg-final { scan-tree-dump "return 475" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
