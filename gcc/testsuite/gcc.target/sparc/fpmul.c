/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef unsigned char pixel __attribute__((vector_size(4)));
typedef short pixel16 __attribute__((vector_size(4)));
typedef unsigned char vec8 __attribute__((vector_size(8)));

vec16 foo1 (pixel a, vec16 b) {
  return __builtin_vis_fmul8x16 (a, b);
}

vec16 foo2 (pixel a, pixel16 b) {
  return __builtin_vis_fmul8x16au (a, b);
}

vec16 foo3 (pixel a, pixel16 b) {
  return __builtin_vis_fmul8x16al (a, b);
}

vec16 foo4 (vec8 a, vec16 b) {
  return __builtin_vis_fmul8sux16 (a, b);
}

vec16 foo5 (vec8 a, vec16 b) {
  return __builtin_vis_fmul8ulx16 (a, b);
}

vec32 foo6 (pixel a, pixel16 b) {
  return __builtin_vis_fmuld8sux16 (a, b);
}

vec32 foo7 (pixel a, pixel16 b) {
  return __builtin_vis_fmuld8ulx16 (a, b);
}

/* { dg-final { scan-assembler "fmul8x16\t%" } } */
/* { dg-final { scan-assembler "fmul8x16au\t%" } } */
/* { dg-final { scan-assembler "fmul8x16al\t%" } } */
/* { dg-final { scan-assembler "fmul8sux16\t%" } } */
/* { dg-final { scan-assembler "fmul8ulx16\t%" } } */
/* { dg-final { scan-assembler "fmuld8sux16\t%" } } */
/* { dg-final { scan-assembler "fmuld8ulx16\t%" } } */
