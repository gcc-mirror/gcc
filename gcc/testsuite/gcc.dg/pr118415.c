/* PR middle-end/118415 */
/* { dg-do run } */
/* { dg-options "-O2" } */

unsigned char crc_table_for_crc_8_polynomial_0x7[2] = { 0xff, 0xff };

static unsigned char
foo (unsigned char byte, unsigned char crc)
{
  unsigned int i;
  crc ^= byte;
  for (i = 0; i < 8; i++)
    crc = (crc << 1) ^ ((crc >> 7) ? 0x07 : 0);
  return crc;
}

int
main ()
{
  volatile unsigned char byte = 1;
  volatile unsigned char crc = 0;
  crc = foo (byte, crc);
  if (__CHAR_BIT__ == 8 && crc != 7)
    __builtin_abort ();
}
