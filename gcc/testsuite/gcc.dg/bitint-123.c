/* PR middle-end/120547 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64 } */

#define CHECK(x, y) \
  if ((_Float64) x != (_Float64) y				\
      || (_Float64) (x + 1) != (_Float64) (y + 1))		\
    __builtin_abort ()

int
main ()
{
  unsigned long long a = 0x20000000000001ULL << 7;
  volatile unsigned long long b = a;
  CHECK (a, b);
#if __BITINT_MAXWIDTH__ >= 4096
  unsigned _BitInt(4096) c = ((unsigned _BitInt(4096)) 0x20000000000001ULL) << 253;
  volatile unsigned _BitInt(4096) d = c;
  CHECK (c, d);
  unsigned _BitInt(4096) e = ((unsigned _BitInt(4096)) 0x20000000000001ULL) << 931;
  volatile unsigned _BitInt(4096) f = e;
  CHECK (e, f);
#endif
}
