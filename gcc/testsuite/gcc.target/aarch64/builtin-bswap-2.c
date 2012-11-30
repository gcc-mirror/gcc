/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times "rev16\\t" 2 } }  */

/* rev16 */
unsigned short
swapu16_1 (unsigned short x)
{
  return (x << 8) | (x >> 8);
}

/* rev16 */
unsigned short
swapu16_2 (unsigned short x)
{
  return (x >> 8) | (x << 8);
}
