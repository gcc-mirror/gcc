/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_usad_char } */
/* With AVX256 or more we do not pull off the trick eliding the epilogue.  */
/* { dg-additional-options "-mprefer-avx128" { target { x86_64-*-* i?86-*-* } } } */

typedef unsigned char uint8_t;
int x264_pixel_sad_8x8( uint8_t *pix1, uint8_t *pix2, int i_stride_pix2 )
{
  int i_sum = 0;
  for( int y = 0; y < 8; y++ )
    {
      i_sum += __builtin_abs( pix1[0] - pix2[0] );
      i_sum += __builtin_abs( pix1[1] - pix2[1] );
      i_sum += __builtin_abs( pix1[2] - pix2[2] );
      i_sum += __builtin_abs( pix1[3] - pix2[3] );
      i_sum += __builtin_abs( pix1[4] - pix2[4] );
      i_sum += __builtin_abs( pix1[5] - pix2[5] );
      i_sum += __builtin_abs( pix1[6] - pix2[6] );
      i_sum += __builtin_abs( pix1[7] - pix2[7] );
      pix1 += 16;
      pix2 += i_stride_pix2;
    }
  return i_sum;
}

/* { dg-final { scan-tree-dump "vect_recog_sad_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump-not "access with gaps requires scalar epilogue loop" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
