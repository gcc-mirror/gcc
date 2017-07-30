/* { dg-do link } */
/* { dg-options "-O -mgeneral-regs-only -mno-cld -g" } */

#include <stdint.h>

extern void link_error (void);

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

struct interrupt_frame
{
  uword_t ip;
  uword_t cs;
  uword_t flags;
  uword_t sp;
  uword_t ss;
};

__attribute__ ((used, interrupt))
void
foo (struct interrupt_frame *frame, uword_t error)
{
  void *ra = __builtin_return_address (0);
  if ((uintptr_t) ra != (uintptr_t) frame->ip)
    link_error ();
}

int
main (void)
{
  return 0;
}
