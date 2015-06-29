typedef int DItype __attribute__ ((mode (DI)));
DItype
__bswapdi2 (DItype u)
{
  return ((((u) & 0xff00000000000000ull) >> 56)
   | (((u) & 0x00ff000000000000ull) >> 40)
   | (((u) & 0x0000ff0000000000ull) >> 24)
   | (((u) & 0x000000ff00000000ull) >> 8)
   | (((u) & 0x00000000ff000000ull) << 8)
   | (((u) & 0x0000000000ff0000ull) << 24)
   | (((u) & 0x000000000000ff00ull) << 40)
   | (((u) & 0x00000000000000ffull) << 56));
}
