/* PR target/98853 */

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
__attribute__((__noipa__)) unsigned long long
foo (unsigned x, unsigned long long y, unsigned long long z)
{
  __builtin_memcpy (2 + (char *) &x, 2 + (char *) &y, 2);
  return x + z;
}
#endif

int
main ()
{
#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (foo (0x44444444U, 0x1111111111111111ULL, 0x2222222222222222ULL)
      != 0x2222222233336666ULL)
    __builtin_abort ();
#endif
  return 0;
}
