/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(4)));

vec16 foo (vec32 a) {
  return __builtin_vis_fpackfix (a);
}

/* { dg-final { scan-assembler "fpackfix\t%" } } */
