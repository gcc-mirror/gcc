extern int gpio_int;
extern int read_reg (int);

void __attribute__ ((interrupt))
isr_handler (void)
{
  gpio_int = read_reg (1);
}
/* { dg-final { scan-assembler-times "\\sblink" 2 } } */
