#if defined (__ARCHS__) || defined (__ARCEM__)
#define RILINK "ilink"
#else
#define RILINK "ilink1"
#endif

extern int gpio_int;
extern int read_reg (int);

void __attribute__ ((interrupt(RILINK)))
isr_handler (void)
{
  gpio_int = read_reg (1);
}
/* { dg-final { scan-assembler-times "blink" 2 } } */
