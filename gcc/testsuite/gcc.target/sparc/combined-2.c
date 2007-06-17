/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ultrasparc -mvis" } */
typedef unsigned char pixel __attribute__((vector_size(4)));
typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));

vec16 foo (pixel a, pixel b) {
  vec8 c = __builtin_vis_fpmerge (a, b);
  vec16 d = { -1, -1, -1, -1 };
  vec16 e = __builtin_vis_fmul8x16 (a, d);

  return e;
}

vec16 bar (pixel a) {
  vec16 d = { 0, 0, 0, 0 };
  vec16 e = __builtin_vis_fmul8x16 (a, d);  /* Mulitplication by 0 = 0.  */

  return e;
}

/* { dg-final { scan-assembler "fmul8x16" } } */
/* { dg-final { scan-assembler "fzero" } } */
