/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef unsigned char pixel __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(4)));

pixel foo (vec8 a, vec8 b) {
  return __builtin_vis_fpmerge (a, b);
}

/* { dg-final { scan-assembler "fpmerge\t%" } } */
