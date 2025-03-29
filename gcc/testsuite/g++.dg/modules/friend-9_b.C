// { dg-additional-options -fmodules }

// Check that f and A are mangled as attached to M.
// void f@M<A@M<main::loc> >(A@M<main::loc>)
// { dg-final { scan-assembler "_ZW1M1fIS_1AIZ4mainE3locEEvT_" } }

import M;

int main()
{
  struct loc {};
  f(A<loc>());
}
