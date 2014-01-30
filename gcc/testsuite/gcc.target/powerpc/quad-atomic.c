/* { dg-do run { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mcpu=power8 -O2" } */

/* Test whether we get the right bits for quad word atomic instructions.  */
#include <stdlib.h>

static __int128_t quad_fetch_and (__int128_t *, __int128_t value) __attribute__((__noinline__));
static __int128_t quad_fetch_or  (__int128_t *, __int128_t value) __attribute__((__noinline__));
static __int128_t quad_fetch_add (__int128_t *, __int128_t value) __attribute__((__noinline__));

static __int128_t
quad_fetch_and (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

static __int128_t
quad_fetch_or (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_ACQUIRE);
}

static __int128_t
quad_fetch_add (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_ACQUIRE);
}

int
main (void)
{
  __int128_t result;
  __int128_t value;
  __int128_t and_input	= ((((__int128_t) 0x1234567890abcdefULL) << 64) | ((__int128_t) 0xfedcba0987654321ULL));
  __int128_t and_value	= ((((__int128_t) 0xfffffffffffffff0ULL) << 64) | ((__int128_t) 0xfffffffffffffff0ULL));
  __int128_t and_exp	= ((((__int128_t) 0x1234567890abcde0ULL) << 64) | ((__int128_t) 0xfedcba0987654320ULL));

  __int128_t or_input	= ((((__int128_t) 0x1234567890abcdefULL) << 64) | ((__int128_t) 0xfedcba0987654321ULL));
  __int128_t or_value	= ((((__int128_t) 0x0000000000000010ULL) << 64) | ((__int128_t) 0x000000000000000eULL));
  __int128_t or_exp	= ((((__int128_t) 0x1234567890abcdffULL) << 64) | ((__int128_t) 0xfedcba098765432fULL));

  __int128_t add_input	= ((((__int128_t) 0x1234567890abcdefULL) << 64) | ((__int128_t) 0xfedcba0987654321ULL));
  __int128_t add_value	= ((((__int128_t) 0x0000000001000000ULL) << 64) | ((__int128_t) 0x0000001000000000ULL));
  __int128_t add_exp	= ((((__int128_t) 0x1234567891abcdefULL) << 64) | ((__int128_t) 0xfedcba1987654321ULL));


  value = and_input;
  result = quad_fetch_and (&value, and_value);
  if (result != and_input || value != and_exp)
    abort ();

  value = or_input;
  result = quad_fetch_or (&value, or_value);
  if (result != or_input || value != or_exp)
    abort ();

  value = add_input;
  result = quad_fetch_add (&value, add_value);
  if (result != add_input || value != add_exp)
    abort ();

  return 0;
}

