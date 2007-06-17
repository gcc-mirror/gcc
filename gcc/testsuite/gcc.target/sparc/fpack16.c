/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(4)));

vec8 foo (vec16 a) {
  return __builtin_vis_fpack16 (a);
}

/* { dg-final { scan-assembler "fpack16\t%" } } */
