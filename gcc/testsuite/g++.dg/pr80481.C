// { dg-do compile { target { i?86-*-* x86_64-*-* }  && { ! *-*-solaris* } } }
// -fopenmp implies -pthread
// { dg-require-effective-target pthread } 
// { dg-options "-Ofast -funroll-loops -fopenmp -march=skylake-avx512" }
// Disabling epilogues until we find a better way to deal with scans.
// { dg-additional-options "--param vect-epilogues-nomask=0" }


#include <math.h>

#include <xmmintrin.h>

#define max(a, b)   ( (a) > (b) ? (a) : (b) )

struct Sdata {
  float w; 
  float s;
  float r;
  float t;
  float v;
};
 extern int N1, N2, N3;

#define func(p, up, down) ((p)*(up) + (1.0f-(p)) * (down))
 
void foo (Sdata *in, int idx, float *out)
{
  float* y1 = (float*)_mm_malloc(sizeof(float) * N1,16);
  float* y2  = (float*)_mm_malloc(sizeof(float) * N1,16);
  float* y3  = (float*)_mm_malloc(sizeof(float) * N1,16);
  float* y4  = (float*)_mm_malloc(sizeof(float) * N1,16); 

  for (int k = idx; k < idx + N3; k++) {
    float x1 = in[k].r;
    float x2    = in[k].s;
    float x3      = in[k].w;
    float x4     = in[k].v;
    float x5         = in[k].t;
    x5 /= N2;
    float u = exp(x4 * sqrt(x5));
    float d = exp(-x4 * sqrt(x5));
    float a = exp(x1 * x5);
    float m = exp(-x1 * x5);
    float p = (a - d) / (u - d);
    y2[0] = x2;
    y3[0] = float(1.f);
    for (int i = 1; i <= N2; i++) {
      y2[i] = u * y2[i - 1];
      y3[i] = d * y3[i - 1];
    }
#pragma omp simd
    for (int i = 0; i <= N2; i++) {
      y1[i] =
        max((x3 - y2[N2 - i] * y3[i]), float(0.f));
    }
    for (int i = N2 - 1; i >= 0; i--) {
#pragma omp simd
      for (int j = 0; j <= i; j++) {
        y4[j] = func(p,y1[j],y1[j+1]) * m;
      }
#pragma omp simd
      for (int j = 0; j <= i; j++) {
        float t1 = y2[i - j] * y3[j];
        float t2 = max(x3 - t1, float(0.f));
        y1[j] = max(t2, y4[j]);
      }
    }
    out[k] = y1[0];
  }
  _mm_free(y1);
  _mm_free(y2);
  _mm_free(y3);
  _mm_free(y4);
}

// { dg-final { scan-assembler-not "vmovaps\[^\n\r]*zmm\[0-9]+,\[^\n\r]*zmm\[0-9]+" } }
