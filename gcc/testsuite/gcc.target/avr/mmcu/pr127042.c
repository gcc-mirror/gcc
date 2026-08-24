/* PR tree-optimization/127042 */
/* { dg-do compile } */
/* { dg-options "-O2 -mmcu=avr2 -fdump-tree-widening_mul" } */

typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* x fits 16 bits: only x * 0xaaab and x * 0xaaaa remain.  */
uint16_t fits (uint16_t x)
{
  return ((uint64_t) x * 0xaaaaaaab) >> 32;
}

/* Neither operand fits: the full four-partial longhand stays.  */
uint32_t nofit (uint32_t x, uint32_t y)
{
  return ((uint64_t) x * (uint64_t) y) >> 32;
}

/* Two multiplies from fits, four from nofit.  */
/* { dg-final { scan-tree-dump-times " \\* " 6 "widening_mul" } } */
