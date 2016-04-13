/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mcpu=power7 -O2 -mfloat128 -static-libgcc" } */

/*
 * Test program to make sure we are getting more precision than the 53 bits we
 * get with IEEE double.
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stddef.h>

#ifndef TYPE
#define TYPE __float128
#endif

#ifndef NO_INLINE
#define NO_INLINE __attribute__((__noinline__))
#endif

static TYPE power_of_two (ssize_t) NO_INLINE;
static TYPE calc1 (TYPE) NO_INLINE;
static TYPE calc2 (TYPE) NO_INLINE;
static TYPE calc3 (TYPE) NO_INLINE;

#ifndef POWER2
#define POWER2 60
#endif


/*
 * Print TYPE in hex.
 */


#if defined(DEBUG) || defined(DEBUG2)
static void print_hex (const char *prefix, TYPE, const char *suffix) NO_INLINE;

#if defined (__i386__) || defined (__x86_64__) || defined (__LITTLE_ENDIAN__)
#define ENDIAN_REVERSE(N, MAX)        ((MAX) - 1 - (N))

#else
#define ENDIAN_REVERSE(N, MAX)        (N)
#endif

static void
print_hex (const char *prefix, TYPE value, const char *suffix)
{
  union {
    TYPE f128;
    unsigned char uc[sizeof (TYPE)];
  } u;

  size_t i;

  u.f128 = value;
  printf ("%s0x", prefix);
  for (i = 0; i < sizeof (TYPE); i++)
    printf ("%.2x", u.uc[ ENDIAN_REVERSE (i, sizeof (TYPE)) ]);

  printf (", %24.2Lf%s", (long double)value, suffix);
}
#endif


/*
 * Return a power of two.
 */

static TYPE
power_of_two (ssize_t num)
{
  TYPE ret = (TYPE) 1.0;
  ssize_t i;

  if (num >= 0)
    {
      for (i = 0; i < num; i++)
	ret *= (TYPE) 2.0;
    }
  else
    {
      ssize_t num2 = -num;
      for (i = 0; i < num2; i++)
	ret /= (TYPE) 2.0;
    }

#ifdef DEBUG
  printf ("power_of_two (%2ld)   = ", (long) num);
  print_hex ("", ret, "\n");
#endif

  return ret;
}


#ifdef ADDSUB
static TYPE add (TYPE a, TYPE b) NO_INLINE;
static TYPE sub (TYPE a, TYPE b) NO_INLINE;

static TYPE
add (TYPE a, TYPE b)
{
  TYPE c;
#ifdef DEBUG
  print_hex ("add, arg1           = ", a, "\n");
  print_hex ("add, arg2           = ", b, "\n");
#endif
  c = a + b;
#ifdef DEBUG
  print_hex ("add, result         = ", c, "\n");
#endif
  return c;
}

static TYPE
sub (TYPE a, TYPE b)
{
  TYPE c;
#ifdef DEBUG
  print_hex ("sub, arg1           = ", a, "\n");
  print_hex ("sub, arg2           = ", b, "\n");
#endif
  c = a - b;
#ifdef DEBUG
  print_hex ("sub, result         = ", c, "\n");
#endif
  return c;
}

#else
#define add(x, y) ((x) + (y))
#define sub(x, y) ((x) - (y))
#endif

/*
 * Various calculations.  Add in 2**POWER2, and subtract 2**(POWER2-1) twice, and we should
 * get the original value.
 */

static TYPE
calc1 (TYPE num)
{
  TYPE num2 = add (power_of_two (POWER2), num);
  TYPE ret;

#ifdef DEBUG
  print_hex ("calc1 (before call) = ", num2, "\n");
#endif

  ret = calc2 (num2);

#ifdef DEBUG
  print_hex ("calc1 (after call)  = ", ret, "\n");
#endif

  return ret;
}

static TYPE
calc2 (TYPE num)
{
  TYPE num2 = sub (num, power_of_two (POWER2-1));
  TYPE ret;

#ifdef DEBUG
  print_hex ("calc2 (before call) = ", num2, "\n");
#endif

  ret = calc3 (num2);

#ifdef DEBUG
  print_hex ("calc2 (after call)  = ", ret, "\n");
#endif

  return ret;
}

static TYPE
calc3 (TYPE num)
{
  TYPE ret = sub (num, (((TYPE) 2.0) * power_of_two (POWER2-2)));

#ifdef DEBUG
  print_hex ("calc3               = ", ret, "\n");
#endif

  return ret;
}


int
main (void)
{
  TYPE input, output;

#ifdef DEBUG
  printf ("Testing, %ld bytes\n", (long) sizeof (TYPE));
#endif

  input = power_of_two (-1);
  if ((double)input != 0.5)
    {
#if defined(DEBUG) || defined(DEBUG2)
      print_hex ("Input should be 0.5:  ", output, "\n");
      return 1;
#else
      __builtin_abort ();
#endif
    }

  output = calc1 (input);
  if ((double)output != 0.5)
    {
#if defined(DEBUG) || defined(DEBUG2)
      print_hex ("Output should be 0.5: ", output, "\n");
      return 1;
#else
      __builtin_abort ();
#endif
    }

  return 0;
}
