// { dg-additional-options {-fmodules-ts -Wno-pedantic} }

import One;
int counter = 0;
Dyn three;
import Two;

int main ()
{
  if (one != 1)
    return 1;
  if (two != 2)
    return 2;
  if (three != 3)
    return 3;
  return 0;
}
