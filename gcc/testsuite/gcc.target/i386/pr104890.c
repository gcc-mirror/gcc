/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mshstk -march=i686" } */

#include <x86gprintrin.h>

__attribute__((target ("general-regs-only")))
int
foo ()
{
  return _get_ssp ();
}
