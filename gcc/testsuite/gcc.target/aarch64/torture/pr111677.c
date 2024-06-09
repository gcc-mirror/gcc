/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-options "-ffast-math -fstack-protector-strong -fopenmp" } */
typedef struct {
  long size_z;
  int width;
} dt_bilateral_t;
typedef float dt_aligned_pixel_t[4];
#pragma omp declare simd
void dt_bilateral_splat(dt_bilateral_t *b) {
  float *buf;
  long offsets[8];
  for (; b;) {
    int firstrow;
    for (int j = firstrow; j; j++)
      for (int i; i < b->width; i++) {
        dt_aligned_pixel_t contrib;
        for (int k = 0; k < 4; k++)
          buf[offsets[k]] += contrib[k];
      }
    float *dest;
    for (int j = (long)b; j; j++) {
      float *src = (float *)b->size_z;
      for (int i = 0; i < (long)b; i++)
        dest[i] += src[i];
    }
  }
}
