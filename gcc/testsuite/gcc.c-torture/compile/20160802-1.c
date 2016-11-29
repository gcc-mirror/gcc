long g (long width, unsigned long byte) {
  long r_hi = 0;
  unsigned long r_lo = 0;
  int s;
  for (s = 0; s < width; s += 8)
    {
      int d = width - s - 8;
      if (s < (8 * 8))
        r_hi |= byte << (d - (8 * 8));
    }
  return r_lo + r_hi;
}

