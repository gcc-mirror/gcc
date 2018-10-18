// { dg-additional-options "-fmodules-ts" }

import frob;

int main ()
{
  A a;

  if (int (a) != 99)
    return 1;
  if (float (a) != 99)
    return 2;
  if (static_cast <void *> (a) != (void *)99)
    return 3;

  B<int> bi(1);
  B<float> bf(1.25f);
  if (int (bi) != 1)
    return 4;
  if (int (bf) != 1)
    return 5;

  // 1.25 is exactly representable
  if (float (bf) != 1.25f)
    return 6;

  return 0;
}
