/* { dg-do run } */

typedef __UINT32_TYPE__ u32;

int
main (void)
{
  u32 b = 0x027C5902;
  u32 a = 0;
  __builtin_memset (1 + (char *) &b, 0, 2);
  __builtin_memcpy (&a, 2 + (char *) &b, 2);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (a != 0x00000200)
#else
  if (a != 0x00020000)
#endif
    __builtin_abort();
  return 0;
}
