/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */

void
foo (double x, char *y, int z)
{
  _Float16 a = (_Float16) x;
  if (!z)
    {
      unsigned short b;
      __builtin_memcpy (&b, &a, 2);
      b = __builtin_bswap16(b);
      __builtin_memcpy (y, &b, 2);
    }
  else
    __builtin_memcpy (y, &a, sizeof (_Float16));
}
