/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(8)));

vec8 foo (vec32 a, vec8 b) {
  return __builtin_vis_fpack32 (a, b);
}

/* { dg-final { scan-assembler "fpack32\t%" } } */
