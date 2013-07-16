/* PR inline-asm/56405 */

void
foo (void)
{
  asm volatile ("" : "+m" (*(volatile unsigned short *) 0x1001UL));
}
