/* PR target/68286 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c;
int fn1 ()
{
  int d[] = {0};
  for (; c; c++)
    {
      float e = c;
      if (e)
        d[0]++;
    }
  b = d[0];
  return a;
}
