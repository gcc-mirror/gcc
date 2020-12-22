// { dg-additional-options -fmodules-ts }

import frob;

int main ()
{
  if (6 != add (1, 2, 3))
    return 1;

  if (10 != sum (1, unsigned (2), 3.0f, 4.0))
    return 2;

  return 0;
}
