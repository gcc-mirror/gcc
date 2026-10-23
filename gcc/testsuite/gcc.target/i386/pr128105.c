/* PR target/128105 */
/* { dg-do compile } */
/* { dg-options "-mvpclmulqdq -mno-sse" } */

int
foo (short s)
{
  return __builtin_crc16_data16 (0, s, 0);
}
