/* PR 34389 */
/* { dg-do compile } */
/* { dg-options "-Wconversion -Wsign-conversion" } */
/* { dg-require-effective-target int32plus } */

short  mask1(short x)
{
  short y = 0x7fff;
  return x & y;
}

short  mask2(short ssx)
{
  short ssy;
  short ssz;

  ssz = ((int)ssx) & 0x7fff;
  ssy = ((int)ssx) | 0x7fff;
  ssz = ((int)ssx) ^ 0x7fff;
  return ssx & 0x7fff;
}

short  mask3(int si, unsigned int ui)
{
  short ss;
  unsigned short us;

  ss = si & 0x7fff;
  ss = si & 0xAAAA; /* { dg-warning "conversion" } */
  ss = ui & 0x7fff;
  ss = ui & 0xAAAA; /* { dg-warning "conversion" } */

  us = si & 0x7fff;
  us = si & 0xAAAA; /* { dg-warning "conversion" } */
  us = ui & 0x7fff;
  us = ui & 0xAAAA; /* 0xAAAA is zero-extended, thus it masks the
		       upper bits of 'ui' making it fit in 'us'.  */

  return ss;
}

short  mask4(int x, int y)
{
  return x & y; /* { dg-warning "conversion" } */
}

short  mask5(int x)
{
  return x & -1; /* { dg-warning "conversion" } */
}

short  mask6(short x)
{
  return x & -1;
}
