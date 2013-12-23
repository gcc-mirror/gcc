/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

__attribute__((noinline, noclone)) long long
mul (long long x, long long y)
{
  return x * y;
}

long long tab[] = {
  0x7fffffffLL, 0x7fffffffLL, 0x3fffffff00000001LL,
  -0x80000000LL, -0x80000000LL, 0x4000000000000000LL,
  0x7fffffffLL, -0x80000000LL, -0x3fffffff80000000LL,
  -0x80000000LL, 0x7fffffffLL, -0x3fffffff80000000LL,
  3LL, 5LL, 15LL,
  -3LL, -9LL, 27LL,
  6LL, -7LL, -42LL,
  -12LL, 13LL, -156LL,
  0x1555555555555555LL, 6LL, 0x7ffffffffffffffeLL,
  -0x1555555555555555LL, -6LL, 0x7ffffffffffffffeLL,
  0x1555555555555555LL, -6LL, -0x7ffffffffffffffeLL,
  -0x1555555555555555LL, 6LL, -0x7ffffffffffffffeLL,
  0x81234567LL, 0xfdbe971fLL, 0x7fffffffbea72879LL,
  -0x81234567LL, -0xfdbe971fLL, 0x7fffffffbea72879LL,
  0x81234567LL, -0xfdbe971fLL, -0x7fffffffbea72879LL,
  -0x81234567LL, 0xfdbe971fLL, -0x7fffffffbea72879LL
};

int
main ()
{
  unsigned int i;
  for (i = 0; i < sizeof (tab) / sizeof (long long); i += 3)
    if (mul (tab[i], tab[i + 1]) != tab[i + 2]
        || mul (tab[i + 1], tab[i]) != tab[i + 2])
      __builtin_abort ();
  return 0;
}
