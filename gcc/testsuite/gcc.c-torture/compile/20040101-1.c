/* { dg-skip-if "not enough registers" { pdp11-*-* } } */

typedef unsigned short uint16_t;
typedef unsigned int uint32_t;

#define CF (1<<0)
#define PF (1<<2)
#define AF (1<<4)
#define ZF (1<<6)
#define SF (1<<7)
#define OF (1<<11)

#define EFLAGS_BITS (CF|PF|AF|ZF|SF|OF)

void test16(uint16_t x, uint32_t eflags)
{
        uint16_t bsr_result;
        uint32_t bsr_eflags;
        uint16_t bsf_result;
        uint32_t bsf_eflags;

        __asm volatile(""
                : "=&r" (bsr_result), "=&r" (bsr_eflags)
                : "r" (x), "i" (~EFLAGS_BITS), "r" (eflags));
        __asm volatile(""
                : "=&r" (bsf_result), "=&r" (bsf_eflags)
                : "r" (x), "i" (~EFLAGS_BITS), "r" (eflags));
        printf("%08x %04x bsrw %02x %08x bsfw %02x %08x\n",
                x, eflags, bsr_result, bsr_eflags, bsf_result, bsf_eflags);
}
