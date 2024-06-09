/* { dg-do compile } */
/* { dg-additional-options "-O3 -ffast-math" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

extern __attribute__((__simd__)) float powf(float, float);
float gv[0][10];
float eq_set_bands_real_adj[0];
void eq_set_bands_real() {
  for (int c = 0; c < 10; c++)
    for (int i = 0; i < 10; i++)
      gv[c][i] = powf(0, eq_set_bands_real_adj[i]) - 1;
}
