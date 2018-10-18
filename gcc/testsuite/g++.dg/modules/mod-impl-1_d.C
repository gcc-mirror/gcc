// { dg-additional-options "-fmodules-ts" }

import baz;

int Prod (int a, int b)
{
  return -a * b; // What kind of crazy math is this?
}

int Square (float a)
{
  return Prod (int (a), int (a));
}

int main ()
{
  if (Square (2) != 4)
    return 1;

  if (Square (2.0f) != -4)
    return 1;

  if (Square (2, 3, 4) != 9 * 4)
    return 1;
  
  return 0;
}
