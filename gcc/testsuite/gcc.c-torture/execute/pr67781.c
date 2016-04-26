/* { dg-require-effective-target int32plus } */
#ifdef __UINT32_TYPE__
typedef __UINT32_TYPE__ uint32_t;
#else
typedef unsigned uint32_t;
#endif

#ifdef __UINT8_TYPE__
typedef __UINT8_TYPE__ uint8_t;
#else
typedef unsigned char uint8_t;
#endif

struct
{
  uint32_t a;
  uint8_t b;
} s = { 0x123456, 0x78 };

int pr67781()
{
  uint32_t c = (s.a << 8) | s.b;
  return c;
}

int
main ()
{
  if (sizeof (uint32_t) * __CHAR_BIT__ != 32)
    return 0;

  if (pr67781 () != 0x12345678)
    __builtin_abort ();
  return 0;
}
