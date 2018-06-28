// { dg-additional-options -fmodules-atom }

import "legacy-2_b.H";

int main ()
{
  if (frob (2) != 4)
    return 1;
  /* Check line number is not disturbed.  */
  if (move () != 6)
    return 2;
  return 0;
}
