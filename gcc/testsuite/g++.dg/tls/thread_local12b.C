// PR c++/60702
// { dg-do run { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

#include "thread_local11.h"

int
main ()
{
  if (*f2 () != 42) abort ();
}
