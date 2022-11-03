/* { dg-do link } */
/* { dg-options "-O0 -ffast-math" } */

int main (void)
{
  float x = 0.123456f;

  float r1 = __builtin_exp2f (x);
  float r2 = __builtin_log2f (x);
}
