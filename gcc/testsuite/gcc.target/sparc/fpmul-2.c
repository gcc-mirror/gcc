/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis -O1 -fdump-tree-optimized" } */

typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(8)));

typedef unsigned char pixel __attribute__((vector_size(4)));
typedef short pixel16 __attribute__((vector_size(4)));

vec16 foo1 () {
  pixel a = { (unsigned char)1, (unsigned char)2, (unsigned char)3, (unsigned char)4 };
  vec16 b = { (short)1, (short)2, (short)3, (short)4 };
  return __builtin_vis_fmul8x16 (a, b);
}

vec16 foo1_1 () {
  pixel a = { (unsigned char)1, (unsigned char)1, (unsigned char)1, (unsigned char)1 };
  vec16 b = { (short)256, (short)512, (short)1024, (short)2048 };
  return __builtin_vis_fmul8x16 (a, b);
}

vec16 foo1_2 () {
  pixel a = { (unsigned char)255, (unsigned char)255, (unsigned char)255, (unsigned char)255 };
  vec16 b = { (short)256, (short)512, (short)1024, (short)32767 };
  return __builtin_vis_fmul8x16 (a, b);
}
/* { dg-final { scan-assembler-not "fmul8x16\t%" } } */
/* { dg-final { scan-tree-dump "{ 0, 0, 0, 0 }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ 1, 2, 4, 8 }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ 255, 510, 1020, 32639 }" "optimized" } } */

vec16 foo2 () {
  pixel a = { 1, 2, 3, 4 };
  pixel16 b = { 256, 512 };
  return __builtin_vis_fmul8x16au (a, b);
}
/* { dg-final { scan-assembler-not "fmul8x16au\t%" } } */
/* { dg-final { scan-tree-dump "{ 1, 2, 3, 4 }" "optimized" } } */

vec16 foo3 () {
  pixel a = { 1, 2, 3, 4 };
  pixel16 b = { 256, 512 };
  return __builtin_vis_fmul8x16al (a, b);
}
/* { dg-final { scan-assembler-not "fmul8x16al\t%" } } */
/* { dg-final { scan-tree-dump "{ 2, 4, 6, 8 }" "optimized" } } */
