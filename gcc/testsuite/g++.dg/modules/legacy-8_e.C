// { dg-additional-options "-fmodules-ts" }

import foo;

int main ()
{
  int (*ifp) (int) = Sqr;
  float (*ffp) (float) = Sqr;

  if (ifp (2) != 4)
    return 1;

  // Comparing these two floats is ok
  if (ffp (2.0f) != 4.0f)
    return 2;

  return 0;
}
