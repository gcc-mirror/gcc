/* { dg-do run } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define BITNO_I	 7
#define BITNO_T	 6
#define BITNO_H	 5
#define BITNO_S	 4
#define BITNO_V	 3
#define BITNO_N	 2
#define BITNO_Z	 1
#define BITNO_C	 0

#define I (1u << BITNO_I)
#define T (1u << BITNO_T)
#define H (1u << BITNO_H)
#define S (1u << BITNO_S)
#define V (1u << BITNO_V)
#define N (1u << BITNO_N)
#define Z (1u << BITNO_Z)
#define C (1u << BITNO_C)

#define bit(a, x) ((bool) ((a) & (1u << (x))))

typedef union
{
  uint8_t val;
  struct
  {
    bool c:1;
    bool z:1;
    bool n:1;
    bool v:1;
    bool s:1;
    bool h:1;
    bool t:1;
    bool i:1;
  };
} sreg_t;


typedef struct
{
  sreg_t sreg;
  uint8_t mask;
  uint16_t result;
} flags_t;

flags_t flags_sub (uint8_t d, uint8_t r)
{
  uint8_t res = d - r;
  bool R7 = bit (res, 7);

  bool Rd7 = bit (d, 7);
  bool Rd3 = bit (d, 3);

  bool R3 = bit (res, 3);
  bool Rr7 = bit (r, 7);
  bool Rr3 = bit (r, 3);

  sreg_t s = { 0 };

  s.v = (Rd7 & !Rr7 & !R7) | (!Rd7 & Rr7 & R7);
  s.n = R7;
  s.z = res == 0;
  s.c = (!Rd7 & Rr7) | (Rr7 & R7) | (R7 & !Rd7);
  s.h = (!Rd3 & Rr3) | (Rr3 & R3) | (R3 & !Rd3);
  s.s = s.n ^ s.v;
  
  return (flags_t) { s, H | S | V | N | Z | C, res };
}

flags_t flags_sbc (uint8_t d, uint8_t r, sreg_t sreg)
{
  uint8_t res = d - r - sreg.c;
  bool R7 = bit (res, 7);

  bool Rd7 = bit (d, 7);
  bool Rd3 = bit (d, 3);

  bool R3 = bit (res, 3);
  bool Rr7 = bit (r, 7);
  bool Rr3 = bit (r, 3);

  sreg_t s = { 0 };

  s.v = (Rd7 & !Rr7 & !R7) | (!Rd7 & Rr7 & R7);
  s.n = R7;
  s.z = (res == 0) & sreg.z;
  s.c = (!Rd7 & Rr7) | (Rr7 & R7) | (R7 & !Rd7);
  s.h = (!Rd3 & Rr3) | (Rr3 & R3) | (R3 & !Rd3);
  s.s = s.n ^ s.v;
  
  return (flags_t) { s, H | S | V | N | Z | C, res };
}

flags_t flags_neg (uint8_t d)
{
  uint8_t res = -d;
  bool R7 = bit (res, 7);
  bool R6 = bit (res, 6);
  bool R5 = bit (res, 5);
  bool R4 = bit (res, 4);
  bool R3 = bit (res, 3);
  bool R2 = bit (res, 2);
  bool R1 = bit (res, 1);
  bool R0 = bit (res, 0);

  bool Rd3 = bit (d, 3);

  sreg_t s = { 0 };

  s.v = R7 & !R6  & !R5	 & !R4	& !R3  & !R2  & !R1  & !R0; 
  s.n = R7;
  s.z = res == 0;
  s.c = R7 | R6 | R5 | R4 | R3 | R2 | R1 | R0;
  s.h = R3 | Rd3;
  s.s = s.n ^ s.v;
  
  return (flags_t) { s, H | S | V | N | Z | C, res };
}

flags_t flags_ror (uint8_t d, sreg_t sreg)
{
  uint8_t res = (d + 0x100 * sreg.c) >> 1;

  sreg_t s = { 0 };

  s.c = bit (d, 0);
  s.z = res == 0;
  s.n = bit (res, 7);
  s.v = s.n ^ s.c;
  s.s = s.n ^ s.v;

  return (flags_t) { s, S | V | N | Z | C, res };
}

flags_t flags_add (uint8_t d, uint8_t r)
{
  uint8_t res = d + r;
  bool R7 = bit (res, 7);

  bool Rd7 = bit (d, 7);
  bool Rd3 = bit (d, 3);

  bool R3 = bit (res, 3);
  bool Rr7 = bit (r, 7);
  bool Rr3 = bit (r, 3);

  sreg_t s = { 0 };

  s.v = (Rd7 & Rr7 & !R7) | (!Rd7 & !Rr7 & R7);
  s.n = R7;
  s.z = res == 0;
  s.c = (Rd7 & Rr7) | (Rr7 & !R7) | (!R7 & Rd7);
  s.h = (Rd3 & Rr3) | (Rr3 & !R3) | (!R3 & Rd3);
  s.s = s.n ^ s.v;
  
  return (flags_t) { s, H | S | V | N | Z | C, res };
}

static inline
sreg_t sreg_sub (uint8_t d, uint8_t r, uint8_t sreg, uint8_t result)
{
  __asm ("out __SREG__,%[sreg]"	 "\n\t"
	 "sub %[d],%[r]"	 "\n\t"
	 "in %[sreg],__SREG__"
	 : [sreg] "+r" (sreg), [d] "+r" (d)
	 : [r] "r" (r));
  if (d != result)
    exit (__LINE__);
  return (sreg_t) sreg;
}

static inline
sreg_t sreg_sbc (uint8_t d, uint8_t r, uint8_t sreg, uint8_t result)
{
  __asm ("out __SREG__,%[sreg]"	 "\n\t"
	 "sbc %[d],%[r]"	 "\n\t"
	 "in %[sreg],__SREG__"
	 : [sreg] "+r" (sreg), [d] "+r" (d)
	 : [r] "r" (r));
  if (d != result)
    exit (__LINE__);
  return (sreg_t) sreg;
}

static inline
sreg_t sreg_neg (uint8_t d, uint8_t sreg, uint8_t result)
{
  __asm ("out __SREG__,%[sreg]"	 "\n\t"
	 "neg %[d]"		 "\n\t"
	 "in %[sreg],__SREG__"
	 : [sreg] "+r" (sreg), [d] "+r" (d));
  if (d != result)
    exit (__LINE__);
  return (sreg_t) sreg;
}

static inline
sreg_t sreg_ror (uint8_t d, uint8_t sreg, uint8_t result)
{
  __asm ("out __SREG__,%[sreg]"	 "\n\t"
	 "ror %[d]"		 "\n\t"
	 "in %[sreg],__SREG__"
	 : [sreg] "+r" (sreg), [d] "+r" (d));
  if (d != result)
    exit (__LINE__);
  return (sreg_t) sreg;
}

static inline
sreg_t sreg_add (uint8_t d, uint8_t r, uint8_t sreg, uint8_t result)
{
  __asm ("out __SREG__,%[sreg]"	 "\n\t"
	 "add %[d],%[r]"	 "\n\t"
	 "in %[sreg],__SREG__"
	 : [sreg] "+r" (sreg), [d] "+r" (d)
	 : [r] "r" (r));
  if (d != result)
    exit (__LINE__);
  return (sreg_t) sreg;
}

void test_sub (uint8_t d, uint8_t r, sreg_t sreg)
{
  sreg_t s0 = sreg_sub (d, r, sreg.val, d - r);
  flags_t f = flags_sub (d, r);
  if ((f.sreg.val & f.mask) != (s0.val & f.mask))
    exit (__LINE__);
}

void test_sbc (uint8_t d, uint8_t r, sreg_t sreg)
{
  sreg_t s0 = sreg_sbc (d, r, sreg.val, d - r - sreg.c);
  flags_t f = flags_sbc (d, r, sreg);
  if ((f.sreg.val & f.mask) != (s0.val & f.mask))
    exit (__LINE__);
}

void test_neg (uint8_t d, sreg_t sreg)
{
  sreg_t s0 = sreg_neg (d, sreg.val, -d);
  flags_t f = flags_neg (d);
  if ((f.sreg.val & f.mask) != (s0.val & f.mask))
    exit (__LINE__);
}

void test_add (uint8_t d, uint8_t r, sreg_t sreg)
{
  sreg_t s0 = sreg_add (d, r, sreg.val, d + r);
  flags_t f = flags_add (d, r);
  if ((f.sreg.val & f.mask) != (s0.val & f.mask))
    exit (__LINE__);
}

void test_ror (uint8_t d, sreg_t sreg)
{
  sreg_t s0 = sreg_ror (d, sreg.val, (d + 0x100 * sreg.c) >> 1);
  flags_t f = flags_ror (d, sreg);
  if ((f.sreg.val & f.mask) != (s0.val & f.mask))
    exit (__LINE__);
}

void test_sreg (void)
{
  uint8_t d = 0;

  do
    {
      uint8_t r = 0;
      test_neg (d, (sreg_t) { 0x00 });
      test_neg (d, (sreg_t) { 0xff });

      test_ror (d, (sreg_t) { 0 });
      test_ror (d, (sreg_t) { C });

      do
	{
	  test_add (d, r, (sreg_t) { 0x00 });
	  test_add (d, r, (sreg_t) { 0xff });

	  test_sub (d, r, (sreg_t) { 0x00 });
	  test_sub (d, r, (sreg_t) { 0xff });

	  test_sbc (d, r, (sreg_t) { 0 });
	  test_sbc (d, r, (sreg_t) { C });
	  test_sbc (d, r, (sreg_t) { Z });
	  test_sbc (d, r, (sreg_t) { C | Z });
	} while (++r);
    } while (++d);
}

int main (void)
{
  test_sreg();
  return 0;
}
