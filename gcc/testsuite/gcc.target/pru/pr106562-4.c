/* Functional test for DI comparisons.  */

/* { dg-do run } */
/* { dg-options "-pedantic-errors" } */

/* The default test suite options use "-ansi", which
   generates spurious errors by enabling "-Wlong-long".
   Thus override the options and drop "-ansi", in order
   to freely use 64-bit (long long) types for PRU.  */

#include <stddef.h>
#include <stdint.h>

extern void abort (void);

char __attribute__((noinline)) test_lt (int64_t a, int64_t b)
{
  return a < b;
}

char __attribute__((noinline)) test_ltu (uint64_t a, uint64_t b)
{
  return a < b;
}

char __attribute__((noinline)) test_le (int64_t a, int64_t b)
{
  return a <= b;
}

char __attribute__((noinline)) test_leu (uint64_t a, uint64_t b)
{
  return a <= b;
}

char __attribute__((noinline)) test_gt (int64_t a, int64_t b)
{
  return a > b;
}

char __attribute__((noinline)) test_gtu (uint64_t a, uint64_t b)
{
  return a > b;
}

char __attribute__((noinline)) test_ge (int64_t a, int64_t b)
{
  return a >= b;
}

char __attribute__((noinline)) test_geu (uint64_t a, uint64_t b)
{
  return a >= b;
}

char __attribute__((noinline)) test_eq (uint64_t a, uint64_t b)
{
  return a == b;
}

char __attribute__((noinline)) test_ne (uint64_t a, uint64_t b)
{
  return a != b;
}

struct test_case {
    uint64_t a;
    uint64_t b;
    char lt;
    char ltu;
    char le;
    char leu;
    char gt;
    char gtu;
    char ge;
    char geu;
    char eq;
    char ne;
};

const struct test_case cases[] = {
  /*                       LT,LTU,LE,LEU,GT,GTU,GE,GEU,EQ,NE */
  { 0x1234567800112233ULL,
    0x1234567800112233ULL, 0,  0, 1,  1, 0,  0, 1,  1, 1, 0 },
  { 0x0000000000000000ULL,
    0x0000000000000000ULL, 0,  0, 1,  1, 0,  0, 1,  1, 1, 0 },
  { 0xffffffffffffffffULL,
    0xffffffffffffffffULL, 0,  0, 1,  1, 0,  0, 1,  1, 1, 0 },

  { 0xffffffffffffffefULL,
    0xffffffffffffffffULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },
  { 0x8000000000000000ULL,
    0xffffffffffffffffULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },
  { 0x80000000ffffffffULL,
    0xffffffffffffffffULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },
  { 0x80000000ffffffffULL,
    0xffffffff00000000ULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },
  { 0xffefffffffffffffULL,
    0xffffffffffffffffULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },

  { 0x0000000000000000ULL,
    0xffffffffffffffffULL, 0,  1, 0,  1, 1,  0, 1,  0, 0, 1 },
  { 0x0000000000000001ULL,
    0xffffffffffffffffULL, 0,  1, 0,  1, 1,  0, 1,  0, 0, 1 },
  { 0x0000000000000001ULL,
    0x8000000000000000ULL, 0,  1, 0,  1, 1,  0, 1,  0, 0, 1 },
  { 0x7fffffffffffffffULL,
    0x8000000000000000ULL, 0,  1, 0,  1, 1,  0, 1,  0, 0, 1 },

  /* Ensure lo uses unsigned comparison if hi parts are same.  */
  { 0x12345678ffffffffULL,
    0x1234567800000001ULL, 0,  0, 0,  0, 1,  1, 1,  1, 0, 1 },
  { 0xf23456780fffffffULL,
    0xf234567800000001ULL, 0,  0, 0,  0, 1,  1, 1,  1, 0, 1 },
  { 0xf2345678ffffffffULL,
    0xf234567800000001ULL, 0,  0, 0,  0, 1,  1, 1,  1, 0, 1 },
  { 0x1234567800000002ULL,
    0x1234567800000001ULL, 0,  0, 0,  0, 1,  1, 1,  1, 0, 1 },
  { 0x1234567800000002ULL,
    0x1234567800000003ULL, 1,  1, 1,  1, 0,  0, 0,  0, 0, 1 },
};

int
main (void)
{
  size_t i;

  for (i = 0; i < (sizeof (cases)/sizeof (cases[0])); i++)
    {
      const int64_t sa = (int64_t)cases[i].a;
      const int64_t sb = (int64_t)cases[i].b;
      const uint64_t ua = cases[i].a;
      const uint64_t ub = cases[i].b;

      if (cases[i].lt != test_lt (sa, sb))
	abort ();
      if (cases[i].ltu != test_ltu (ua, ub))
	abort ();
      if (cases[i].le != test_le (sa, sb))
	abort ();
      if (cases[i].leu != test_leu (ua, ub))
	abort ();
      if (cases[i].gt != test_gt (sa, sb))
	abort ();
      if (cases[i].gtu != test_gtu (ua, ub))
	abort ();
      if (cases[i].ge != test_ge (sa, sb))
	abort ();
      if (cases[i].geu != test_geu (ua, ub))
	abort ();
      if (cases[i].eq != test_eq (ua, ub))
	abort ();
      if (cases[i].ne != test_ne (ua, ub))
	abort ();
    }

  return 0;
}

