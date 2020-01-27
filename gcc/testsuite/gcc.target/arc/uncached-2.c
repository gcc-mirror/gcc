/* { dg-do compile } */

void clkgen_switch(unsigned int base, unsigned int offset, int val)
{
  volatile  unsigned int __attribute__ ((uncached)) *dest =
    (volatile unsigned int __attribute__ ((uncached)) *) (base + offset);
  *dest = val;
}
/* { dg-final { scan-assembler-times "st\.di" 2 } } */
