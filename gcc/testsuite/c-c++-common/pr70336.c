/* PR c++/70336 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */

void
f1 (unsigned char * x, int y, int z)
{
  x[z / 8] |= (unsigned char) (0x80 >> y);	/* { dg-bogus "may alter its value" } */
}

unsigned char
f2 (unsigned char x, int y)
{
  x = x | (unsigned char) (0x80 >> y);		/* { dg-bogus "may alter its value" } */
  return x;
}

unsigned char
f3 (unsigned char x, int y)
{
  x = x | (unsigned char) (y & 255);		/* { dg-bogus "may alter its value" } */
  return x;
}

unsigned char
f4 (unsigned char x, unsigned char y)
{
  x = x | (unsigned char) (y & 255);		/* { dg-bogus "may alter its value" } */
  return x;
}

unsigned char
f5 (unsigned char x, int y)
{
  x = (unsigned char) (y & 255);		/* { dg-bogus "may alter its value" } */
  return x;
}
