/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-additional-options "-O3" } */

#define N 64

/* The counting exit is tested before the early exit, so the loop body runs
   one time less than the loop header.  The vector loop must not process a
   lane for the iteration that only tests the counter, as that lane reads the
   element next to the arrays.  */

__attribute__ ((noipa)) int
differs (const long *a, const long *b, unsigned long n)
{
  while (n != 0)
    {
      n--;
      if (a[n] != b[n])
	return 1;
    }
  return 0;
}

__attribute__ ((noipa)) int
differs_signed (const long *a, const long *b, long n)
{
  if (n < 0)
    return 2;
  while (--n >= 0)
    {
      if (a[n] != b[n])
	return 1;
    }
  return 0;
}

__attribute__ ((noipa)) int
differs_up (const long *a, const long *b, unsigned long n)
{
  unsigned long i = 0;
  while (i != n)
    {
      unsigned long j = i;
      i++;
      if (a[j] != b[j])
	return 1;
    }
  return 0;
}

/* The store before the early exit must not write the extra lane and must run
   once for every element.  */

__attribute__ ((noipa)) int
bump (const long *restrict a, const long *restrict b, long *restrict c,
      unsigned long n)
{
  while (n != 0)
    {
      n--;
      c[n] += 1;
      if (a[n] != b[n])
	return 1;
    }
  return 0;
}

/* On the normal exit, the scalar epilogue must not repeat the stores from
   the final vector iteration.  */

__attribute__ ((noipa)) int
bump_up (const long *restrict a, const long *restrict b, long *restrict c,
	 unsigned long n)
{
  unsigned long i = 0;
  while (i != n)
    {
      unsigned long j = i++;
      c[j] += 1;
      if (a[j] != b[j])
	return 1;
    }
  return 0;
}

static long abuf[N + 16] __attribute__ ((aligned (64)));
static long bbuf[N + 16] __attribute__ ((aligned (64)));
static long cbuf[N + 16] __attribute__ ((aligned (64)));

int
main (void)
{
  for (unsigned long n = 0; n <= N; n++)
    for (int off = 0; off < 8; off++)
      {
	/* Start the arrays at every alignment of a 64 byte vector and give
	   the elements around them different values.  */
	long *a = abuf + 8 + off;
	long *b = bbuf + 8 + off;
	long *c = cbuf + 8 + off;
	for (unsigned long i = 0; i < n; i++)
	  a[i] = b[i] = c[i] = i * 3 + 1;
	a[-1] = 1;
	b[-1] = 2;
	c[-1] = 5;
	a[n] = 3;
	b[n] = 4;
	c[n] = 6;

	if (differs (a, b, n) != 0 || differs_signed (a, b, n) != 0
	    || differs_up (a, b, n) != 0)
	  __builtin_abort ();

	if (bump (a, b, c, n) != 0 || c[-1] != 5 || c[n] != 6)
	  __builtin_abort ();
	for (unsigned long i = 0; i < n; i++)
	  if (c[i] != i * 3 + 2)
	    __builtin_abort ();

	for (unsigned long i = 0; i < n; i++)
	  c[i] = i * 3 + 1;
	if (bump_up (a, b, c, n) != 0 || c[-1] != 5 || c[n] != 6)
	  __builtin_abort ();
	for (unsigned long i = 0; i < n; i++)
	  if (c[i] != i * 3 + 2)
	    __builtin_abort ();

	if (n > 0)
	  {
	    b[n / 2]++;
	    if (differs (a, b, n) != 1 || differs_signed (a, b, n) != 1
		|| differs_up (a, b, n) != 1)
	      __builtin_abort ();
	    for (unsigned long i = 0; i < n; i++)
	      c[i] = i * 3 + 1;
	    if (bump_up (a, b, c, n) != 1 || c[-1] != 5 || c[n] != 6)
	      __builtin_abort ();
	    for (unsigned long i = 0; i < n; i++)
	      if (c[i] != i * 3 + 1 + (i <= n / 2))
		__builtin_abort ();
	  }
      }

  return 0;
}
