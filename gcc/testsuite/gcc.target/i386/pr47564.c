/* PR target/47564 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

static inline unsigned long long
foo (const unsigned char *p)
{
  return 1;
}

__attribute__ ((__target__ ("sse4"))) void
bar (unsigned long long *x, const void *b, long long m)
{
  const unsigned char *p = (const unsigned char *) b;
  const unsigned char *e = p + m;
  unsigned int l = *x;
  unsigned long long n = l;

  if ((e - p) >= 8192)
    {
      while ((e - p) >= 128)
	{
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	  n = __builtin_ia32_crc32di (n, foo (p));
	}
    }

  while ((e - p) >= 16)
    {
      n = __builtin_ia32_crc32di (n, foo (p));
      n = __builtin_ia32_crc32di (n, foo (p));
    }
  l = n;
  *x = l;
}
