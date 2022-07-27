/* { dg-do compile } */
/* { dg-options "-O2 -mshstk -mkl" } */

#include <x86gprintrin.h>

__attribute__((target("no-mmx,no-sse")))
int
foo ()
{
  return _get_ssp ();
}
