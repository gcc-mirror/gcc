// { dg-do run }
// { dg-shouldfail "asan" }

#include <complex.h>

static __attribute__ ((noinline)) long double
goo (long double _Complex *a)
{
  return crealf(*(volatile _Complex long double *)a);
}

__attribute__ ((noinline)) float
foo (float _Complex arg)
{
  return goo ((long double _Complex *)&arg);
}

int
main ()
{
  return foo (3 + 2 * I);
}

// { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size \[0-9\]* at.*" }
// { dg-output ".*'arg' \\(line 13\\) <== Memory access at offset \[0-9\]* partially overflows this variable.*" }
