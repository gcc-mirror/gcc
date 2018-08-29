/* { dg-options "-std=gnu99 -Tavr51-flash1.x" } */
/* { dg-do run { target { ! avr_tiny } } } */

#include <stdlib.h>

const __flash char c0 = 1;
const __flash1 char c1 = 1;

int main (void)
{
  const __memx void *p;

  p = &c0;
  if (__builtin_avr_flash_segment (p) != 0)
    abort();

  p = &c1;
  if (__builtin_avr_flash_segment (p) != 1)
    abort();

  if (__builtin_avr_flash_segment ("p") != -1)
    abort();

  exit (0);
  return 0; 
}
