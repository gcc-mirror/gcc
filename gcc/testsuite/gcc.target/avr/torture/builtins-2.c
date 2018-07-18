/* { dg-options "-std=gnu99" } */
/* { dg-do run } */

#include <stdlib.h>

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

int main (void)
{
  f1();
  __builtin_avr_delay_cycles (1000);

  exit (0);
  return 0; 
}
