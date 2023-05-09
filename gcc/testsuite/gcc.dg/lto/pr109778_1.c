int
bar (int x)
{
  x &= 0x22222222;
  x |= (int) 0xf1234567U;
  return x;
}
