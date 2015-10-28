/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler "sbi 0x1f,0" } } */
/* { dg-final { scan-assembler "cbi 0x1f,0" } } */
/* { dg-final { scan-assembler-not "sbi 0x20,0" } } */
/* { dg-final { scan-assembler-not "cbi 0x20,0" } } */
/* { dg-final { scan-assembler "in r\\d+,__SREG__" } } */
/* { dg-final { scan-assembler "out __SREG__,r\\d+" } } */
/* { dg-final { scan-assembler-not "in r\\d+,0x40" } } */
/* { dg-final { scan-assembler-not "out 0x40, r\\d+" } } */

/* This testcase verifies that SBI/CBI/SBIS/SBIC
   and IN/OUT instructions are not generated for
   an IO addresses outside the valid range.
*/
#define IO_ADDR(x) (*((volatile char *)x + __AVR_SFR_OFFSET__))
int main ()
{
  IO_ADDR(0x1f) |= 1;
  IO_ADDR(0x1f) &= 0xFE;

  IO_ADDR(0x20) |= 1;
  IO_ADDR(0x20) &= 0xFE;

  IO_ADDR(0x3f) = IO_ADDR(0x3f);

  IO_ADDR(0x40) = IO_ADDR(0x40);
  return 0;
}
