/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */

typedef long long int64_t;
typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef char vec8 __attribute__((vector_size(8)));

vec16 foo1 (vec16 a, vec16 b) {
  return __builtin_vis_faligndatav4hi (a, b);
}

vec32 foo2 (vec32 a, vec32 b) {
  return __builtin_vis_faligndatav2si (a, b);
}

vec8 foo3 (vec8 a, vec8 b) {
  return __builtin_vis_faligndatav8qi (a, b);
}

int64_t foo4 (int64_t a, int64_t b) {
  return __builtin_vis_faligndatadi (a, b);
}

unsigned char * foo5 (unsigned char *data) {
  return __builtin_vis_alignaddr (data, 0);
}

/* { dg-final { scan-assembler-times "faligndata" 4 } } */
/* { dg-final { scan-assembler "alignaddr.*%g0" } } */
