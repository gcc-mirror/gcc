typedef unsigned short uint16;
f (unsigned char *w)
{
  w[2] = (uint16) ((((g (0) % 10000 + 42) & 0xFF) << 8) | (((g (0) % 10000 + 42) >> 8) & 0xFF)) & 0xFF,
  w[3] = (uint16) ((((g (0) % 10000 + 42) & 0xFF) << 8) | (((g (0) % 10000 + 42) >> 8) & 0xFF)) >> 8;
}
