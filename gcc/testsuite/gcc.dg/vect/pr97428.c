/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" { target riscv*-*-* } } */

typedef struct { double re, im; } dcmlx_t;
typedef struct { double re[4], im[4]; } dcmlx4_t;

void foo_i2(dcmlx4_t dst[], const dcmlx_t src[], int n)
{
  for (int i = 0; i < n; ++i) {
    dcmlx_t s00 = src[i*4+0];
    dcmlx_t s01 = src[i*4+1];
    dcmlx_t s02 = src[i*4+2];
    dcmlx_t s03 = src[i*4+3];

    dcmlx_t s10 = src[i*4+0+n];
    dcmlx_t s11 = src[i*4+1+n];
    dcmlx_t s12 = src[i*4+2+n];
    dcmlx_t s13 = src[i*4+3+n];

    dst[i*2+0].re[0] = s00.re;
    dst[i*2+0].re[1] = s01.re;
    dst[i*2+0].re[2] = s02.re;
    dst[i*2+0].re[3] = s03.re;
    dst[i*2+0].im[0] = s00.im;
    dst[i*2+0].im[1] = s01.im;
    dst[i*2+0].im[2] = s02.im;
    dst[i*2+0].im[3] = s03.im;

    dst[i*2+1].re[0] = s10.re;
    dst[i*2+1].re[1] = s11.re;
    dst[i*2+1].re[2] = s12.re;
    dst[i*2+1].re[3] = s13.re;
    dst[i*2+1].im[0] = s10.im;
    dst[i*2+1].im[1] = s11.im;
    dst[i*2+1].im[2] = s12.im;
    dst[i*2+1].im[3] = s13.im;
  }
}

/* The first step to produce optimal code is to appropriately detect the
   load and store groups.  */
/* { dg-final { scan-tree-dump "Detected interleaving load of size 8" "vect" } } */
/* { dg-final { scan-tree-dump "Detected interleaving store of size 16" "vect" } } */
/* We're not able to peel & apply re-aligning to make accesses well-aligned for !vect_hw_misalign,
   but we could by peeling the stores for alignment and applying re-aligning loads.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail { ! vect_hw_misalign } } } } */
/* { dg-final { scan-tree-dump-not "gap of 6 elements" "vect" } } */
