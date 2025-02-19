/* PR middle-end 118288 */
/* { dg-do compile }  */
/* { dg-options "-O2" } */

signed char crc8_data8 ()
{
  return __builtin_crc8_data8 ('a', 0xff, 0x12);
}
