typedef __UINT8_TYPE__ u8;
typedef __UINT32_TYPE__ u32;

u32 a, b, d, e;
u8 c;

static u32 __attribute__ ((noinline, noclone))
foo (u32 p)
{
  do
    {
      e /= 0xfff;
      if (p > c)
	d = 0;
      e -= 3;
      e *= b <= a;
    }
  while (e >= 88030);
  return e;
}

int
main (void)
{
  u32 x = foo (1164);
  if (x != 0xfd)
    __builtin_abort ();
  return 0;
}


