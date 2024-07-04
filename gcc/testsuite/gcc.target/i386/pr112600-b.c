/* PR target/112600 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "sbb|cmov" 4 } } */

unsigned char
sub_sat_char (unsigned char x, unsigned char y)
{
  unsigned char res;
  res = x - y;
  res &= -(x >= y);
  return res;
}

unsigned short
sub_sat_short (unsigned short x, unsigned short y)
{
  unsigned short res;
  res = x - y;
  res &= -(x >= y);
  return res;
}

unsigned int
sub_sat_int (unsigned int x, unsigned int y)
{
  unsigned int res;
  res = x - y;
  res &= -(x >= y);
  return res;
}

unsigned long
sub_sat_long (unsigned long x, unsigned long y)
{
  unsigned long res;
  res = x - y;
  res &= -(x >= y);
  return res;
}
