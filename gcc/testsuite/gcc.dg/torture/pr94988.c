/* { dg-do run } */

short *b;

void __attribute__((noipa))
bar (short x, int j)
{
  for (int i = 0; i < j; ++i)
    *b++ = x;
}

int
main()
{
  b = (short *)&b;
  bar (0, 1);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if ((short)(__UINTPTR_TYPE__)b != 0)
    __builtin_abort ();
#endif
  return 0;
}
