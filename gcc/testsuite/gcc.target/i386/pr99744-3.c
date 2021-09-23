/* { dg-do compile } */
/* { dg-options "-O2 -mno-serialize" } */

#include <x86intrin.h>

__attribute__ ((target("general-regs-only")))
void
foo1 (void)
{
  _serialize ();
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0 } */
