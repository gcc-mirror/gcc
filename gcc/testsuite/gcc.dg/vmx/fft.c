/* { dg-do compile } */
#include <altivec.h>

inline void
transpose4x4(vector float *matrix)
{
  vector float v0, v1, v2, v3;

  v0 = vec_mergeh(matrix[0], matrix[2]);
  v1 = vec_mergel(matrix[0], matrix[2]);
  v2 = vec_mergeh(matrix[1], matrix[3]);
  v3 = vec_mergel(matrix[1], matrix[3]);

  matrix[0] = vec_mergeh(v0, v2);
  matrix[1] = vec_mergel(v0, v2);
  matrix[2] = vec_mergeh(v1, v3);
  matrix[3] = vec_mergel(v1, v3);
}

void
vec_ifft64(vector float *x0, vector float *x1)
{
  int i;
  vector float real[4], imag[4];
  vector float c0r, c1r, c2r, c3r, c0i, c1i, c2i, c3i;
  vector float d0r, d1r, d2r, d3r, d0i, d1i, d2i, d3i;

  /*
   *  N=64
   *
   *  Stage 1: t=1 => k = 0, j = 0..15
   *  ================================
   *  for j = 0:15
   *    c0 = x0(j+0*16);
   *    c1 = x0(j+1*16);
   *    c2 = x0(j+2*16);
   *    c3 = x0(j+3*16);
   *
   *    d0 = c0 + c2;
   *    d1 = c0 - c2;
   *    d2 = c1 + c3;
   *    d3 = i*(c1 - c3);
   *
   *    x1(4j+0) = d0 + d2;
   *    x1(4j+1) = d1 + d3;
   *    x1(4j+2) = d0 - d2;
   *    x1(4j+3) = d1 - d3;
   *  end
   ******************************************************/

  for (i=0; i < 4; i++)
    {
      c0r = x0[i];
      c1r = x0[i+4];
      c2r = x0[i+8];
      c3r = x0[i+12];

      c0i = x0[i+16];
      c1i = x0[i+20];
      c2i = x0[i+24];
      c3i = x0[i+28];

      d0r = vec_add(c0r, c2r);
      d1r = vec_sub(c0r, c2r);
      d2r = vec_add(c1r, c3r);
      d3r = vec_sub(c3i, c1i);

      d0i = vec_add(c0i, c2i);
      d1i = vec_sub(c0i, c2i);
      d2i = vec_add(c1i, c3i);
      d3i = vec_sub(c1r, c3r);

      /* Calculate real{x1} */
      real[0] = vec_add(d0r, d2r);
      real[1] = vec_add(d1r, d3r);
      real[2] = vec_sub(d0r, d2r);
      real[3] = vec_sub(d1r, d3r);

      transpose4x4(real);

      /* Calculate imag{x1} */
      imag[0] = vec_add(d0i, d2i);
      imag[1] = vec_add(d1i, d3i);
      imag[2] = vec_sub(d0i, d2i);
      imag[3] = vec_sub(d1i, d3i);

      transpose4x4(imag);

      x1[4*i]   = real[0];
      x1[4*i+1] = real[1];
      x1[4*i+2] = real[2];
      x1[4*i+3] = real[3];

      x1[4*i+16] = imag[0];
      x1[4*i+17] = imag[1];
      x1[4*i+18] = imag[2];
      x1[4*i+19] = imag[3];
    }
}
