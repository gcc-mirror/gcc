expand_to_ascii (int *i, int *o)
{
  unsigned x, y, out;
  unsigned x1;

  /* Big endian code.  */

  x = *i++;

  y = x >> (32 - 13);
  out = (y / 91);
  out = (out << 8) | (y % 91);

  x <<= 13;
  y = x >> (32 - 13);
  out = (out << 8) | (y / 91);
  out = (out << 8) | (y % 91);

  *o++ = out + 0x20202020;

  /* 6 bits left in x.  */

  x1 = *i++;
  x = (x << 26) | (x1 >> 6);
}
