// { dg-additional-options "-fmodules -fdump-lang-module" }

// Test that we clean up the unpopped change in M.
// { dg-final { scan-lang-dump {Adding final pop} module } }

import N;
import M;

int main()
{
  f({24,42});
}
