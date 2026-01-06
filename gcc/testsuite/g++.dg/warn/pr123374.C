// { dg-do compile }
// { dg-additional-options "-O2 -Wstringop-overflow" }

long ext_f (void *, unsigned long)
    __attribute__ ((__access__ (__write_only__, 1, 2)));

long f (void *b, unsigned long n)
{
  unsigned long sz = __builtin_dynamic_object_size(b, 0);

  return (__builtin_constant_p (sz) && sz == -1UL) ? ext_f (b, n) : 0;
}


void test (unsigned limit, long init_off)
{
  char buf[4096];
  unsigned long off = 0;

  while (off == 0)
    off += init_off;

  while (off < limit)
    {
      long n = f (buf + off, sizeof (buf) - off);

      if (n <= 0)
	continue;

      off += n;
    }
}
