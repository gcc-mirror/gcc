/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mshstk -masm=att" } */

#include <immintrin.h>
#include <stdint.h>

int
foo (uint64_t *ss_slot, uint64_t val)
{
  if (*ss_slot == val)
    return 1;
  _wrssq (val, ss_slot);
  return (*ss_slot == val);
}

/* The load after wrssq must not be CSE'd with the one before it,
   so the comparison must be emitted twice.  */
/* { dg-final { scan-assembler-times "\[ \t\]cmpq" 2 } } */
