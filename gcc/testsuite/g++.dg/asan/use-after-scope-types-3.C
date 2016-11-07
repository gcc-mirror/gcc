// { dg-do run }
// { dg-shouldfail "asan" }

#include "use-after-scope-types.h"

int main()
{
  using Tests = void (*)();
  Tests t = &test<void *>;
  t();

  return 0;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "WRITE of size " }
// { dg-output ".*'x' <== Memory access at offset \[0-9\]* is inside this variable.*" }
