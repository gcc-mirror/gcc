/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic" } */

char *jpeg_difference7_input_buf;
void
jpeg_difference7 (int *diff_buf)
{
  unsigned width;
  int samp, Rb;
  while (--width)
    {
      Rb = samp = *jpeg_difference7_input_buf;
      *diff_buf++ = -(int) (samp + (long) Rb >> 1);
    }
}
