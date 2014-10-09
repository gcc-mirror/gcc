unsigned
good (unsigned src, unsigned o5, unsigned w5)
{
  return src & ~((w5 == 0) ? (~0 << o5) : (1 << o5));
}

unsigned
bad (unsigned src, unsigned o5, unsigned w5)
{
  return src & ((w5 == 0) ? ~(~0 << o5) : ~(1 << o5));
}

