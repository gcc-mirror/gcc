// { dg-do compile }
// { dg-options "-O3" }
// { dg-additional-options "-march=bdver2" { target x86_64-*-* } }

int max(int __b) {
  if (0 < __b)
    return __b;
  return 0;
}
struct Plane {
  Plane(int, int);
  int *Row();
};
#ifdef __x86_64__
#pragma GCC target "sse2,ssse3,avx,avx2"
#endif
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
