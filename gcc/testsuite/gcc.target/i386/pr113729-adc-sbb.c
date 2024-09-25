/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapx-features=ndd -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "movz"} } */

#define FC(TYPE1, TYPE2, OP_NAME, OP)               \
TYPE1                                               \
__attribute__ ((noipa))                             \
fc_##OP_NAME##_##TYPE2##_##TYPE1 (TYPE2 *a, TYPE2 b)\
{                                                   \
  unsigned TYPE2 c = (*a OP b OP (a > b)); /* { dg-warning "comparison between pointer and integer" } */ \
  return c;                                         \
}			

/* addqi3_carry_zext<mode> */
FC (char, short, adc, +)
FC (char, int, adc, +)
FC (char, long, adc, +)
FC (short, int, adc, +)
FC (short, long, adc, +)
/* subqi3_carry_zext<mode> */
FC (char, short, sbb, -)
FC (char, int, sbb, -)
FC (char, long, sbb, -)
FC (short, int, sbb, -)
FC (short, long, sbb, -)

#define FC0(TYPE1, TYPE2, OP_NAME, OP1, OP2)        \
unsigned TYPE2                                      \
__attribute__ ((noipa))                             \
fc0_##OP_NAME##_##TYPE2##_##TYPE1                   \
(unsigned TYPE1 a, unsigned TYPE1 b, TYPE1 c)       \
{                                                   \
  unsigned TYPE1 d = (c OP1 (a OP2 b));             \
  return d;                                         \
}			

/* addqi3_carry_zext<mode>_0 */
FC0 (char, short, adc, +, <)
FC0 (char, int, adc, +, <)
FC0 (char, long, adc, +, <)
FC0 (short, int, adc, +, <)
FC0 (short, long, adc, +, <)
/* subqi3_carry_zext<mode>_0 */
FC0 (char, short, sbb, -, <)
FC0 (char, int, sbb, -, <)
FC0 (char, long, sbb, -, <)
FC0 (short, int, sbb, -, <)
FC0 (short, long, sbb, -, <)
/* subsi3_carry_zext<mode>_0 */
FC0 (int, long, sbb, -, <)
/* addqi3_carry_zext<mode>_0r */
FC0 (char, short, adcr, +, >=)
FC0 (char, int, adcr, +, >=)
FC0 (char, long, adcr, +, >=)
FC0 (short, int, adcr, +, >=)
FC0 (short, long, adcr, +, >=)
/* subqi3_carry_zext<mode>_0r */
FC0 (char, short, sbbr, -, >=)
FC0 (char, int, sbbr, -, >=)
FC0 (char, long, sbbr, -, >=)
FC0 (short, int, sbbr, -, >=)
FC0 (short, long, sbbr, -, >=)
FC0 (int, long, sbbr, -, >=)
