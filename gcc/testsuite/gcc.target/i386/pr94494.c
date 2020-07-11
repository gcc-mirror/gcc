/* PR target/94494 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse -mno-sse2" } */

void
foo (float *item, float *f, float *out,
     int threshold, int wi, int lo, int hi, int value)
{
  for (int i = 0; i < wi; i++) {
    if (item[i] > 0) {
      int found = 0;

      for (int k = lo; k < hi; k++)
        if (f[k] > 0)
          found = 1;

      if (found > 0)
        out[i] = threshold;
      else if (out[i] > value)
        out[i] -= 1;
    }
  }
}
