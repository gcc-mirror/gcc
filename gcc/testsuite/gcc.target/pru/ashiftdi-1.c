/* Functional test for DI left shift.  */

/* { dg-do run } */
/* { dg-options "-pedantic-errors" } */

#include <stddef.h>
#include <stdint.h>

extern void abort (void);

uint64_t __attribute__((noinline)) ashift_1 (uint64_t a)
{
  return a << 1;
}

uint64_t __attribute__((noinline)) ashift_10 (uint64_t a)
{
  return a << 10;
}

uint64_t __attribute__((noinline)) ashift_32 (uint64_t a)
{
  return a << 32;
}

uint64_t __attribute__((noinline)) ashift_36 (uint64_t a)
{
  return a << 36;
}

int
main (int argc, char** argv)
{
  if (ashift_1 (0xaaaa5555aaaa5555ull) != 0x5554aaab5554aaaaull)
    abort();
  if (ashift_10 (0xaaaa5555aaaa5555ull) != 0xa95556aaa9555400ull)
    abort();
  if (ashift_32 (0xaaaa5555aaaa5555ull) != 0xaaaa555500000000ull)
    abort();
  if (ashift_36 (0xaaaa5555aaaa5555ull) != 0xaaa5555000000000ull)
    abort();

  if (ashift_1 (0x1234567822334455ull) != 0x2468acf0446688aaull)
    abort();
  if (ashift_10 (0x1234567822334455ull) != 0xd159e088cd115400ull)
    abort();
  if (ashift_32 (0x1234567822334455ull) != 0x2233445500000000ull)
    abort();
  if (ashift_36 (0x1234567822334455ull) != 0x2334455000000000ull)
    abort();

  return 0;
}
