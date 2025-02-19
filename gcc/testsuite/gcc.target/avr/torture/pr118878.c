/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=c99 } } */

#ifdef __AVR_HAVE_LPMX__

/* From include <avr/pgmspace.h> */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef uint32_t uint_farptr_t;

#define pgm_read_dword_far(__addr) __ELPM_dword (__addr)

#define __ELPM_dword(addr)                        \
  (__extension__({                                \
      uint_farptr_t __addr32 = (addr);            \
      uint32_t __result;                          \
      __ELPM__4 (__result, __addr32, uint32_t);   \
      __result; }))

/* Has no ELPM: Fallback to LPM. */
#define __ELPM__4(r,a,T) const T *__a = (const T*)(uint16_t) a; __LPM__4(r,__a)

#define __LPM__4(res,addr)              \
  __asm volatile ("lpm %A0,%a1+" "\n\t" \
                  "lpm %B0,%a1+" "\n\t" \
                  "lpm %C0,%a1+" "\n\t" \
                  "lpm %D0,%a1+" : "=r" (res), "+z" (addr))

#define PROGMEM __attribute__((__progmem__))

#define pgm_get_far_address(var)              \
  (__extension__({ uint_farptr_t __tmp;       \
      __asm__ ("ldi    %A0, lo8(%1)" "\n\t"   \
               "ldi    %B0, hi8(%1)" "\n\t"   \
               "ldi    %C0, hh8(%1)" "\n\t"   \
               "clr    %D0" : "=d" (__tmp) : "i" (&(var)) ); __tmp; }))

/*********************************************/

#define VAL 0x01050711

PROGMEM
const uint32_t data[] = { VAL, 2 * VAL, 7 * VAL };

uint32_t get_val (uint8_t i)
{
  uint32_t v = VAL;
  if (i == 1) v *= 2;
  if (i == 2) v *= 7;
  return v;
}

__attribute__((noinline,noclone))
void test (uint8_t i)
{
  if (pgm_read_dword_far (pgm_get_far_address (data[0])) != get_val (0))
    __builtin_exit (__LINE__);

  uint_farptr_t pf = pgm_get_far_address (data[0]) + i * sizeof (uint32_t);
  if (pgm_read_dword_far (pf) != get_val (i))
    __builtin_exit (__LINE__);
}

int main (void)
{
  test (1);
  test (2);
  return 0;
}

#else
int main (void)
{
  return 0;
}
#endif
