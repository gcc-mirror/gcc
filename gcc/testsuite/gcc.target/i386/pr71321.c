/* { dg-do compile } */
/* { dg-options "-mtune=generic -O2" } */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;

unsigned cvt_to_2digit(uint8_t i, uint8_t base)
{
  return ((i / base) | (uint32_t)(i % base)<<8);
}
unsigned cvt_to_2digit_ascii(uint8_t i)
{
  return cvt_to_2digit(i, 10) + 0x0a3030;
}
/* { dg-final { scan-assembler-not "lea\[^\n\r]*0" } } */
