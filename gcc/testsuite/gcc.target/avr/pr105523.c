/* { dg-do compile } */
/* { dg-options "-Os -Wall" } */

/* Verify no "array subscript 0 is outside array bounds of" is generated
   for accessing memory addresses in the 0-4096 range. */

typedef __UINT8_TYPE__ uint8_t;

#define SREG (*(volatile uint8_t*) (0x3F + __AVR_SFR_OFFSET__ ))

void bar (void)
{
    SREG = 0;
}
