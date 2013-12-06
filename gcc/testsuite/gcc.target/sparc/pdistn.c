/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis3" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));

long foo (vec8 a, vec8 b) {
  return __builtin_vis_pdistn (a, b);
}

/* { dg-final { scan-assembler-times "pdistn\t%" 1 } } */
