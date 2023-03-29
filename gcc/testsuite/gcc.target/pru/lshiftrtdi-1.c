/* Functional test for DI right shift.  */

/* { dg-do run } */
/* { dg-options "-pedantic-errors" } */

#include <stddef.h>
#include <stdint.h>

extern void abort (void);

uint64_t __attribute__((noinline)) lshift_1 (uint64_t a)
{
  return a >> 1;
}

uint64_t __attribute__((noinline)) lshift_10 (uint64_t a)
{
  return a >> 10;
}

uint64_t __attribute__((noinline)) lshift_32 (uint64_t a)
{
  return a >> 32;
}

uint64_t __attribute__((noinline)) lshift_36 (uint64_t a)
{
  return a >> 36;
}

int
main (int argc, char** argv)
{
  if (lshift_1 (0xaaaa5555aaaa5555ull) != 0x55552aaad5552aaaull)
    abort();
  if (lshift_10 (0xaaaa5555aaaa5555ull) != 0x002aaa95556aaa95ull)
    abort();
  if (lshift_32 (0xaaaa5555aaaa5555ull) != 0x00000000aaaa5555ull)
    abort();
  if (lshift_36 (0xaaaa5555aaaa5555ull) != 0x000000000aaaa555ull)
    abort();

  if (lshift_1 (0x1234567822334455ull) != 0x091a2b3c1119a22aull)
    abort();
  if (lshift_10 (0x1234567822334455ull) != 0x00048d159e088cd1ull)
    abort();
  if (lshift_32 (0x1234567822334455ull) != 0x0000000012345678ull)
    abort();
  if (lshift_36 (0x1234567822334455ull) != 0x0000000001234567ull)
    abort();

  return 0;
}
