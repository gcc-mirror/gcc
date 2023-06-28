/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <assert.h>
int result = 0;

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("leafy")))
foo1 (int x)
{
  return (x + 1);
}

int 
__attribute__((noipa))
__attribute__ ((zero_call_used_regs("leafy")))
foo2 (int x)
{
  return foo1 (x + 2);
}
