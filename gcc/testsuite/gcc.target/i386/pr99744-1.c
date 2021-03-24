/* { dg-do compile } */
/* { dg-options "-O0" } */

#include <x86intrin.h>

extern unsigned long long int curr_deadline;
extern void bar (void);

__attribute__ ((target("general-regs-only")))
void
foo (void)
{
  if (__rdtsc () < curr_deadline)
    return; 
  bar ();
}
