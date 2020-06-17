/* { dg-do compile } */
/* { dg-options "-mtrack-speculation" } */

int synths_ ( float * rc)
{
  float r1, r2;
  int i;
  for (i = 0; i < 128; ++i)
    {
      r2 = rc[i];
      r1 = ((r2) <= (.99f) ? (r2) : (.99f));
      rc[i] = ((r1) >= (-.99f) ? (r1) : (-.99f));
    }
}
