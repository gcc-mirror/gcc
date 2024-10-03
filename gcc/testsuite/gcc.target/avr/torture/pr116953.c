unsigned foo (unsigned x, unsigned y)
{
  int i;
  for (i = 8; i--; x <<= 1)
    y ^= (x ^ y) & 0x80 ? 79U : 0U;
  return y;
}
