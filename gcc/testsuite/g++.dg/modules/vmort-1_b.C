// { dg-additional-options "-fmodules-ts" }

import tom.riddle;

int main ()
{
  auto one = One (2);

  if (int (one) != 2)
    return 1;

  int two = Two (3);
  if (two != 3)
    return 2;

  return 0;
}
