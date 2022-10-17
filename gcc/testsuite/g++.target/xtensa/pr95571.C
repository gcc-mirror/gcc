/* { dg-do run } */

extern "C" void abort(void);
extern "C" void __xtensa_libgcc_window_spill(void);

#ifdef __XTENSA_CALL0_ABI__
void __xtensa_libgcc_window_spill(void)
{
}
#endif

static int call;
static int cnt;

extern "C" void *memcpy(void *dst, const void *src, unsigned int sz)
{
  char *a = (char *)dst;
  const char *b = (const char *)src;

  if (call++ == cnt)
    __xtensa_libgcc_window_spill();

  while (sz--)
    *a++ = *b++;

  return dst;
}

int main()
{
  int i;

  for (i = 0; i < 100; ++i)
    {
      call = 0;
      cnt = i;

      try
	{
	  throw 1;
	}
      catch (int v)
	{
	  if (v != 1)
	    abort ();
	}
    }
  return 0;
}
