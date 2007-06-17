/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(8)));

void foo (vec16 a) {
  __builtin_vis_fpack16 (a);
}

/* { dg-final { scan-assembler-not "fpack16\t%" } } */
