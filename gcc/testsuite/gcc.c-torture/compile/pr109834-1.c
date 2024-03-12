int f (char v)
{
  return __builtin_popcount((int)__builtin_bswap16(v));
}
