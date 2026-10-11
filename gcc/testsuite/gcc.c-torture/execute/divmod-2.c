/* 64-bit signed division and modulo where both operands sit at the extremes
   of the type.

   divmod-1.c does this for signed char and signed short using -1 as the
   divisor, and cannot go wider: char and short promote to int, so
   (signed char) -128 / -1 and (signed short) -32768 / -1 are evaluated in
   int and are representable, whereas INT_MIN / -1 and LLONG_MIN / -1 are
   not, and are therefore undefined -- as is % -1 at the minimum, since C
   ties the one to the other.  So the divisor has to change rather than just
   the type, and the pairs below are chosen for a different hazard.

   None of them is undefined.  A signed division or modulo is undefined
   exactly when the quotient is not representable, and for 64 bits the only
   such pair is LLONG_MIN / -1, which would be 2^63.  Every quotient below is
   -1, 0 or 1; -1 is deliberately never used as a divisor.  Checked under
   -fsanitize=undefined at -O0 and -O2, which is silent.

   These matter to any target whose 64-bit helpers do radix-2 restoring
   division.  Such a helper holds a partial remainder in one 64-bit register,
   and when the divisor is close to 2^63 that remainder can reach 2^63 -- the
   one point where a keep-or-skip decision taken from the sign bit of a
   subtraction could disagree with the unsigned comparison it stands in for.
   No other test in the suite pairs a dividend near 2^63 with a divisor near
   2^63, so nothing else reaches that state.  */

#define LLONG_MIN (-__LONG_LONG_MAX__ - 1)
#define LLONG_MAX __LONG_LONG_MAX__

extern void abort (void);

static long long __attribute__ ((noinline, noclone))
mod (long long a, long long b)
{
  return a % b;
}

static long long __attribute__ ((noinline, noclone))
divide (long long a, long long b)
{
  return a / b;
}

int
main (void)
{
  /* volatile so the operands are not folded away: the point is to reach the
     target's 64-bit helpers at run time.  */
  volatile long long min = LLONG_MIN, max = LLONG_MAX;

  if (mod (min, max) != -1)
    abort ();
  if (mod (min, -max) != -1)
    abort ();
  if (mod (min, min) != 0)
    abort ();
  if (mod (max, min) != max)
    abort ();
  if (mod (min, max - 1) != -2)
    abort ();
  if (mod (min, min + 1) != -1)
    abort ();

  if (divide (min, max) != -1)
    abort ();
  if (divide (min, -max) != 1)
    abort ();
  if (divide (min, min) != 1)
    abort ();
  if (divide (max, min) != 0)
    abort ();

  return 0;
}
