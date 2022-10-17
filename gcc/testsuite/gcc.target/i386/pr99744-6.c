/* { dg-do compile } */
/* { dg-options "-O" } */

#include <x86intrin.h>

extern unsigned long long int curr_deadline;
extern void bar (void);

void
foo1 (void)
{
  if (__rdtsc () < curr_deadline)
    return; 
  bar ();
}

void
foo2 (unsigned int *p)
{
  if (__rdtscp (p) < curr_deadline)
    return; 
  bar ();
}
