/* { dg-options "-std=gnu99 -Tavr51-flash1.x" } */
/* { dg-do run } */

#include <stdlib.h>
#include "../progmem.h"

int volatile a;

void f1 (void) 
{
  __builtin_avr_sei (); 
  __builtin_avr_cli (); 
  __builtin_avr_wdr (); 
  __builtin_avr_sleep (); 
  __builtin_avr_nop ();
  a = __builtin_avr_swap (a);
  a = __builtin_avr_fmul (1,a);
  a = __builtin_avr_fmuls (1,a);
  a = __builtin_avr_fmulsu (1,a);
  a = __builtin_avr_insert_bits (0x1f2f5364, a, a);
}

const __flash char c0 = 1;
const __flash1 char c1 = 1;

int main (void) 
{
  const __memx void *p;

  f1();
  __builtin_avr_delay_cycles (1000);

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
