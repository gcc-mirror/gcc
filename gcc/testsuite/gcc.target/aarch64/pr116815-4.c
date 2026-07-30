/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR middle-end/116815 */

/* The max/min-of-add/sub patterns split into a flag-setting ADDS/SUBS
   followed by a CSEL, so they have to declare that they clobber the
   condition codes.  Without that clobber the compare feeding an enclosing
   CCMP chain is treated as still live across the insn and gets deleted,
   so the wrong value is selected.  */

#pragma GCC target "+nocssc"

__attribute__ ((noipa)) unsigned
umax_plus (unsigned a, unsigned b, unsigned c, unsigned d)
{
  unsigned s = a + b;
  unsigned m = s > a ? s : a;
  return (c < d && a < b) ? m : d;
}

__attribute__ ((noipa)) unsigned
umin_plus (unsigned a, unsigned b, unsigned c, unsigned d)
{
  unsigned s = a + b;
  unsigned m = s < a ? s : a;
  return (c < d && a < b) ? m : d;
}

__attribute__ ((noipa)) unsigned
umax_minus (unsigned a, unsigned b, unsigned c, unsigned d)
{
  unsigned s = a - b;
  unsigned m = s > a ? s : a;
  return (c < d && a < b) ? m : d;
}

__attribute__ ((noipa)) unsigned
umin_minus (unsigned a, unsigned b, unsigned c, unsigned d)
{
  unsigned s = a - b;
  unsigned m = s < a ? s : a;
  return (c < d && a < b) ? m : d;
}

__attribute__ ((noipa)) unsigned long long
umax_plus_di (unsigned long long a, unsigned long long b,
	      unsigned long long c, unsigned long long d)
{
  unsigned long long s = a + b;
  unsigned long long m = s > a ? s : a;
  return (c < d && a < b) ? m : d;
}

__attribute__ ((noipa)) unsigned long long
umin_minus_di (unsigned long long a, unsigned long long b,
	       unsigned long long c, unsigned long long d)
{
  unsigned long long s = a - b;
  unsigned long long m = s < a ? s : a;
  return (c < d && a < b) ? m : d;
}

int
main (void)
{
  /* c < d is false, so every call must return d.  */
  if (umax_plus (5, 7, 9, 2) != 2)
    __builtin_abort ();
  if (umin_plus (5, 7, 9, 2) != 2)
    __builtin_abort ();
  if (umax_minus (5, 7, 9, 2) != 2)
    __builtin_abort ();
  if (umin_minus (5, 7, 9, 2) != 2)
    __builtin_abort ();
  if (umax_plus_di (5, 7, 9, 2) != 2)
    __builtin_abort ();
  if (umin_minus_di (5, 7, 9, 2) != 2)
    __builtin_abort ();

  /* a < b is false, so these must return d too.  */
  if (umax_plus (7, 5, 1, 2) != 2)
    __builtin_abort ();
  if (umax_minus (7, 5, 1, 2) != 2)
    __builtin_abort ();

  /* Both true: the max/min result is selected.  */
  if (umax_plus (5, 7, 1, 2) != 12)
    __builtin_abort ();
  if (umin_plus (5, 7, 1, 2) != 5)
    __builtin_abort ();

  return 0;
}
