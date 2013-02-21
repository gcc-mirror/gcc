/* PR middle-end/56420 */
/* { dg-do run { target int128 } } */

extern void abort (void);

__attribute__((noinline, noclone)) __uint128_t
foo (__uint128_t x)
{
  return x * (((__uint128_t) -1) << 63);
}

__attribute__((noinline, noclone)) __uint128_t
bar (__uint128_t x)
{
  return x * (((__uint128_t) 1) << 63);
}

__attribute__((noinline, noclone)) __uint128_t
baz (__uint128_t x)
{
  return x * -(((__uint128_t) 1) << 62);
}

int
main ()
{
  if (foo (1) != (((__uint128_t) -1) << 63)
      || foo (8) != (((__uint128_t) -1) << 66))
    abort ();
  if (bar (1) != (((__uint128_t) 1) << 63)
      || bar (8) != (((__uint128_t) 1) << 66))
    abort ();
  if (baz (1) != -(((__uint128_t) 1) << 62)
      || baz (8) != ((-(((__uint128_t) 1) << 62)) << 3))
    abort ();
  return 0;
}
