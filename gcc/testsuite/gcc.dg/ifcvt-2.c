/* { dg-do compile { target aarch64*-*-* x86_64-*-* } } */
/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-unpredictable-cost=100" } */


typedef unsigned char uint8_t;
typedef unsigned int uint16_t;

uint8_t
_xtime (const uint8_t byte, const uint16_t generator)
{
  if (byte & 0x80)
    return byte ^ generator;
  else
    return byte << 1;
}

/* { dg-final { scan-rtl-dump "3 true changes made" "ce1" } } */
