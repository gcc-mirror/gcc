/* PR target/108292 */

typedef unsigned V __attribute__((__vector_size__ (64)));

V x;

int
main ()
{
  if (sizeof (unsigned) * __CHAR_BIT__ != 32)
    return 0;
  __builtin_sub_overflow (0, 6, &x[5]);
  x >>= ((V){} != x) & 31;
  for (unsigned i = 0; i < 16; i++)
    if (x[i] != (i == 5))
      __builtin_abort ();
  return 0;
}
