// { dg-additional-options "-fmodules-ts" }

import "legacy-2_b.H";

int main ()
{
  if (frob (2) != 4)
    return 1;
  /* Check line number is not disturbed.  */
  if (move () != 8)
    return 2;
  return 0;
}
