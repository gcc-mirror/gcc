unsigned f(unsigned flags, unsigned capabilities)
{
  unsigned gfp_mask;
  unsigned gfp_notmask = 0;
  gfp_mask = flags & ((1 << 25) - 1);
  if (!(capabilities & 0x00000001))
    gfp_mask |= 0x1000000u;
  return (gfp_mask & ~gfp_notmask);
}
