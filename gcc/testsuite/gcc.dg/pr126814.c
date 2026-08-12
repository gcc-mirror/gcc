/* { dg-do compile } */
/* { dg-options "-O2 -funsafe-math-optimizations" } */

float r;
void use (int);

void
check (int n)
{
  for (int j = 0; j < n; j++)
    {
      int y = (int) __builtin_ceilf (j * r);
      if (y >= n)
        y = -n;
      use (y * 4);
    }
}
