/* __builtin_ms_va_list is only supported for x86_64 -m64.  */
/* { dg-skip-if "" { ! {x86_64-*-* && { ! ilp32 } } } } */

#include <stdio.h>

int __attribute__((ms_abi)) va_demo (int count, ...);

int
main (void)
{
  printf ("sum == %d\n", va_demo (5, 1, 2, 3, 4, 5));
  return 0;
}
