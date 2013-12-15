/* Test for cross x86_64<->w64 abi standard calls.  */
/* { dg-options "-mabi=ms -std=gnu99 -ffast-math -fno-builtin -maccumulate-outgoing-args" } */

long double func_cross (long double a, double b, float c, long d, int e,
			char f)
{
  long double ret;
  ret = a + (long double) b + (long double) c;
  ret *= (long double) (d + (long) e);
  if (f>0)
    ret += func_cross (a,b,c,d,e,-f);
  return ret;
}
