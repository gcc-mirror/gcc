/* { dg-do compile } */
/* { dg-options "-O2 -mshstk -mwidekl" } */

#include <x86gprintrin.h>

__attribute__((target("no-mmx,no-sse")))
int
foo ()
{
  return _get_ssp ();
}
