/* Exercise the mode policy with different element widths.  */

void
add_byte (unsigned char *restrict dst, const unsigned char *restrict src,
	  unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = src[i] + 1;
}

void
add_halfword (unsigned short *restrict dst, const unsigned short *restrict src,
	      unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = src[i] + 1;
}

void
add_word (unsigned *restrict dst, const unsigned *restrict src, unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = src[i] + 1;
}
