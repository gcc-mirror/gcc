/* { dg-do link } */
/* { dg-options "-O2 -mbmi" } */

#include <x86intrin.h>

/* Test that a constant operand 0 to tzcnt gets folded.  */
extern void link_error(void);
int main()
{
  if (__tzcnt_u32(0) != 32)
    link_error();
  return 0;
}
