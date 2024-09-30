/* PR middle-end/116358 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

long long f(int b, int c, long long d)
{
  if (c) {
    long long bb = b;
    long long t2 = (bb < 16 ? bb : 16);
    d =  t2 - 16;
  }
  return d;
}

/* { dg-final { scan-assembler-not "bl" } } */
