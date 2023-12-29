/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-n2 -fdump-tree-vect-details" } */

typedef struct {
  short blue, green, red, opacity;
} Pixel;

double foo (long n, double *k, Pixel *k_pixels) {
  double result_2, result_1, result_0;
  for (; n; n++, k--) {
    result_0 += *k * k_pixels[n].red;
    result_1 += *k * k_pixels[n].green;
    result_2 += *k * k_pixels[n].blue;
  }
  return result_0 + result_1 + result_2;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
