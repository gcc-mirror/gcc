/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis -O1 -fdump-tree-final_cleanup" } */
typedef unsigned char pixel __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(4)));

#define _(ARG) (unsigned char)ARG

pixel foo () {
  vec8 a = { _(1), _(3), _(5), _(7) };
  vec8 b = { _(2), _(4), _(6), _(8) };
  return __builtin_vis_fpmerge (a, b);
}

/* { dg-final { scan-assembler-not "fpmerge\t%" } } */
/* { dg-final { scan-tree-dump "{ 1, 2, 3, 4, 5, 6, 7, 8 }" "final_cleanup" } } */
