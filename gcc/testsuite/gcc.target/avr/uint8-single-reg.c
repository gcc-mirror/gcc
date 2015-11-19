/* { dg-do compile } */
/* { dg-options "-Os" } */

/* This testcase verifies that a uint8_t variable assigned from a wider variable
   with the same range is held in a single register. VRP must not fold away the
   conversion and use two regs to hold the uint16_t - widenings are ok only upto 
   word mode (1 byte for AVR).
*/

unsigned int foo(const unsigned int wvalue)
{
  const unsigned char type = (wvalue >> 8);
  unsigned int size = 0;

  if (type == 1)
  {
    size = 20;
  }
  return size;
}

/* { dg-final { scan-assembler "cpi r25,lo8\\(1\\)" } } */
/* { dg-final { scan-assembler-not "cpc r\\d+,__zero_reg__" } } */

