/* { dg-do compile } */
/* { dg-additional-options { -std=c99 } } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT8_TYPE__ uint8_t;

#define PINB (*(volatile uint8_t*) (13 + __AVR_SFR_OFFSET__))
#define PB1 1

uint16_t fun (void)
{
    uint16_t h = 0;
    for (uint32_t s = 0; s < 0x10000; ++s)
        h += (PINB >> PB1) & 1;
    return h;
}
