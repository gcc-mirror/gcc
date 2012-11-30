/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times "rev16\\t" 2 } }  */

/* rev16 */
short
swaps16 (short x)
{
  return __builtin_bswap16 (x);
}

/* rev16 */
unsigned short
swapu16 (unsigned short x)
{
  return __builtin_bswap16 (x);
}
