/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(8)));
typedef char vec8 __attribute__((vector_size(4)));

vec16 foo (vec8 a) {
  return __builtin_vis_fexpand (a);
}

/* { dg-final { scan-assembler "fexpand\t%" } } */
