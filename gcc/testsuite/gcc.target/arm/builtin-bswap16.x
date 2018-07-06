unsigned short swapu16_1 (unsigned short x)
{
  return (x << 8) | (x >> 8);
}

unsigned short swapu16_2 (unsigned short x)
{
  return (x >> 8) | (x << 8);
}
