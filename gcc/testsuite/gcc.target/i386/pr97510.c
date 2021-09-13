/* PR target/97510 */
/* { dg-do compile } */
/* { dg-options "-Os -fexcess-precision=standard -mfpmath=387 -funsafe-math-optimizations" } */

float compute_rsqrt_ref_r_0;

__attribute__((optimize(1)))
void compute_rsqrt_ref() {
  compute_rsqrt_ref_r_0 = 1.0 / 0.0;
}

int icompute_rsqrt_ref(float *);

void test_512() {
  float in[0];
  for (int i;;)
    in[i] = 8.6756 * icompute_rsqrt_ref(in);
}
