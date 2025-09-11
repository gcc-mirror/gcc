/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mcpu=neoverse-v2" } */

float a;
void
fn1 (int b)
{
  for (; b < 10; b++)
    {
      a = 01.;
      for (int c = 0; c < 2000; c++)
        a *= 0.99;
    }
}
