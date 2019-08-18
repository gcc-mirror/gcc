/* { dg-require-effective-target vect_usad_char } */

#include "tree-vect.h"

typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

extern int abs (int);
extern void abort (void);

int __attribute__((noinline,noclone))
foo (uint8_t *pix1, uint8_t *pix2, int i_stride_pix2)
{
  int i_sum = 5;
  for( int y = 0; y < 16; y++ )
    {
      i_sum += abs ( pix1[0] - pix2[0] );
      i_sum += abs ( pix1[1] - pix2[1] );
      i_sum += abs ( pix1[2] - pix2[2] );
      i_sum += abs ( pix1[3] - pix2[3] );
      i_sum += abs ( pix1[4] - pix2[4] );
      i_sum += abs ( pix1[5] - pix2[5] );
      i_sum += abs ( pix1[6] - pix2[6] );
      i_sum += abs ( pix1[7] - pix2[7] );
      i_sum += abs ( pix1[8] - pix2[8] );
      i_sum += abs ( pix1[9] - pix2[9] );
      i_sum += abs ( pix1[10] - pix2[10] );
      i_sum += abs ( pix1[11] - pix2[11] );
      i_sum += abs ( pix1[12] - pix2[12] );
      i_sum += abs ( pix1[13] - pix2[13] );
      i_sum += abs ( pix1[14] - pix2[14] );
      i_sum += abs ( pix1[15] - pix2[15] );
      pix1 += 16;
      pix2 += i_stride_pix2;
    }
  return i_sum; 
}

int
main ()
{
  check_vect ();

  uint8_t X[16*16];
  uint8_t Y[16*16];

  for (int i = 0; i < 16*16; ++i)
    {
      X[i] = i;
      Y[i] = 16*16 - i;
      __asm__ volatile ("");
    }

  if (foo (X, Y, 16) != 32512 + 5)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "sad pattern recognized" "vect" } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
