/* { dg-do compile } */

typedef struct {
  float real;
  float imag;
} complex_t;
extern unsigned char fftorder[];
float *a52_imdct_256_data;
int a52_imdct_256_i, a52_imdct_256_k;
float a52_imdct_256_b_r;
void a52_imdct_256()
{
  complex_t buf1[64];
  a52_imdct_256_i = 0;
  for (; a52_imdct_256_i < 64; a52_imdct_256_i++) {
    a52_imdct_256_k = fftorder[a52_imdct_256_i];
    buf1[a52_imdct_256_i].real = buf1[a52_imdct_256_i].imag =
        a52_imdct_256_data[a52_imdct_256_k];
  }
  a52_imdct_256_b_r = buf1[0].real * buf1[0].imag;
}
