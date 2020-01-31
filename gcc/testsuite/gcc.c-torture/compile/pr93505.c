/* PR middle-end/93505 */

unsigned a;

unsigned
foo (unsigned x)
{
  unsigned int y = 32 - __builtin_bswap64 (-a);
  /* This would be UB (x << 32) at runtime.  Ensure we don't
     invoke UB in the compiler because of that (visible with
     bootstrap-ubsan).  */
  x = x << y | x >> (-y & 31);
  x >>= 31;
  return x;
}
