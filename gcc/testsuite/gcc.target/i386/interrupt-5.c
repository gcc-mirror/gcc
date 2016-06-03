/* { dg-do link } */
/* { dg-options "-O -mgeneral-regs-only -mno-cld -g" } */

#include <stdint.h>

extern void link_error (void);

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

__attribute__ ((used, interrupt))
void
foo (void *frame, uword_t error)
{
  void *ra = __builtin_return_address (0);
  if ((uintptr_t) ra != (uintptr_t) error)
    link_error ();
}

int
main (void)
{
  return 0;
}
