// { dg-additional-options -fmodules-atom }

import "err-2_a.H" // { dg-error "expected" }

int main ()
{
  // Error here to make sure main is parsed
  return; // { dg-error "" }
}
