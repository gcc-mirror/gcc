/* PR middle-end/127031: synthesizing a wide multiply by a dense
   constant overflowed struct algorithm's operation arrays (sized
   MAX_BITS_PER_WORD = 8 on AVR) and crashed in synth_mult.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __UINT32_TYPE__ uint32_t;

uint32_t f (uint32_t x)
{
  return x * 0xaaab;
}
