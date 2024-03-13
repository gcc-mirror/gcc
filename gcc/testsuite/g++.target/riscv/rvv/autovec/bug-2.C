/* { dg-options "-march=rv64gc_zve32f -mabi=lp64d -O3 -mrvv-max-lmul=m4" } */

int max(int __b) {
  if (0 < __b)
    return __b;
  return 0;
}
struct Plane {
  Plane(int, int);
  int *Row();
};
float *ConvolveXSampleAndTranspose_rowp;
int ConvolveXSampleAndTranspose_res, ConvolveXSampleAndTranspose_r;
void ConvolveXSampleAndTranspose() {
  Plane out(0, ConvolveXSampleAndTranspose_res);
  for (int y;;) {
    float sum;
    for (int i = ConvolveXSampleAndTranspose_r; i; ++i)
      sum += i;
    for (; ConvolveXSampleAndTranspose_r; ++ConvolveXSampleAndTranspose_r)
      sum +=
          ConvolveXSampleAndTranspose_rowp[max(ConvolveXSampleAndTranspose_r)] *
          ConvolveXSampleAndTranspose_r;
    out.Row()[y] = sum;
  }
}
