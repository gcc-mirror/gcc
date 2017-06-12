#if __SIZEOF_INT__ < 4
  __extension__ typedef __UINT32_TYPE__ uint32_t;
  __extension__ typedef __INT32_TYPE__ int32_t;
#else
  typedef unsigned uint32_t;
  typedef int int32_t;
#endif

extern void abort (void);

__attribute__ ((noinline, noclone)) unsigned long long f1 (int32_t x)
{
  return ((unsigned long long) x) << 4;
}

__attribute__ ((noinline, noclone)) long long f2 (uint32_t x)
{
  return ((long long) x) << 4;
}

__attribute__ ((noinline, noclone)) unsigned long long f3 (uint32_t x)
{
  return ((unsigned long long) x) << 4;
}

__attribute__ ((noinline, noclone)) long long f4 (int32_t x)
{
  return ((long long) x) << 4;
}

int main ()
{
  if (f1 (0xf0000000) != 0xffffffff00000000)
    abort ();
  if (f2 (0xf0000000) != 0xf00000000)
    abort ();
  if (f3 (0xf0000000) != 0xf00000000)
    abort ();
  if (f4 (0xf0000000) != 0xffffffff00000000)
    abort ();
  return 0;
}
