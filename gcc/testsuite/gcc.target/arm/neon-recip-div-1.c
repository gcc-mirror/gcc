/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O3 -freciprocal-math -fno-unsafe-math-optimizations -save-temps" } */
/* { dg-add-options arm_neon } */

int *a;
int n;
void b() {
  int c;
  for (c = 0; c < 100000; c++)
    a[c] = (float)c / n;
}
/* We should not ICE, or get a vectorized reciprocal instruction when unsafe
   math optimizations are disabled.  */
/* { dg-final { scan-assembler-not "vrecpe\\.f32\\t\[qd\].*" } } */
/* { dg-final { scan-assembler-not "vrecps\\.f32\\t\[qd\].*" } } */
