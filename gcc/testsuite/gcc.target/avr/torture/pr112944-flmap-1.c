/* { dg-do run } */
/* { dg-additional-options "-Wl,--defsym,__flmap=1" } */

const int val = 1234;

#ifdef __AVR_HAVE_FLMAP__
/* Initialize NVMCTRL_CTRLB.FLMAP to __flmap.  */
#include <avr/io.h>
__attribute__((naked, section(".init2")))
__attribute__((used, unused, no_instrument_function))
static void init_flmap (void)
{
  uint8_t ctrlb = NVMCTRL_CTRLB;
  ctrlb &= ~ NVMCTRL_FLMAP_gm;
  ctrlb |= 1 << NVMCTRL_FLMAP_gp;
  NVMCTRL_CTRLB = ctrlb;
}
#endif

int main (void)
{
  const int *p = & val;
  __asm ("" : "+r" (p));

  if (*p != 1234)
    __builtin_abort ();

  return 0;
}
