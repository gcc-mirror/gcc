/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O2" } */

/* Generate sum of absolute difference as sub (max, min).
   This helps with x264 sad routines.  */

inline int abs(int i)
{
  return (i < 0 ? -i : i);
}

int pixel_sad_n(unsigned char *pix1, unsigned char *pix2, int n)
{
  int sum = 0;
  for( int i = 0; i < n; i++ )
       sum += abs(pix1[i] - pix2[i]);

  return sum;
}

/* { dg-final { scan-assembler {vminu\.v} } } */
/* { dg-final { scan-assembler {vmaxu\.v} } } */
/* { dg-final { scan-assembler {vsub\.v} } } */
