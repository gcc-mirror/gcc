// Test case to check if multiversioning works as expected when the versions
// are defined in different files. Auxiliary file for mv12.C.

#include "mv12.h"

__attribute__ ((target ("sse4.2")))
int foo ()
{
  return 1;
}
