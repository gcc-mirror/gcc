/* PR target/119428 */

__attribute__((noipa)) void
foo (unsigned int x, unsigned char *y)
{
  y += x >> 3;
  *y &= (unsigned char) ~(1 << (x & 0x07));
}

int
main ()
{
  unsigned char buf[8];
  __builtin_memset (buf, 0xff, 8);
  foo (8, buf);
  if (buf[1] != 0xfe)
    __builtin_abort ();
}
