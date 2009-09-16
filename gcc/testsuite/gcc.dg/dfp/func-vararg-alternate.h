/* Simple test of vararg passing for problematic types with and without
   double values passed between them.  */

#include "dfp-dbg.h"
#include <stdarg.h>

DTYPE a[10];
double b[10];

union U {
  DTYPE d;
  unsigned int i[INTS];
};

void
compare (double r, double s, int *p, int *q, int n, int line)
{
  int i;

  for (i = 0; i < n; i++)
    if (r != s || p[i] != q[i])
#ifdef DBG
      {
	int j;

	failures++;
	printf ("line %-3d", line);
	for (j = 0; j < n; j++)
	  printf ("  %08x", p[j]);
	printf ("    %10.2g\n        ", r);
	for (j = 0; j < n; j++)
	  printf ("  %08x", q[j]);
	printf ("    %10.2g\n\n", s);
	    
	return;
      }
#else
      __builtin_abort ();
#endif
}

void
bar0 (int n, ...)
{
  union U u;
  int j;
  va_list ap;

  va_start (ap, n);
  for (j = 0; j < n; j++)
    a[j] = va_arg (ap, DTYPE);
  va_end (ap);
}

void
bar1 (int n, ...)
{
  union U u;
  int j;
  va_list ap;

  va_start (ap, n);
  for (j = 0; j < n; j++)
    {
      a[j] = va_arg (ap, DTYPE);
      b[j] = va_arg (ap, double);
    }
  va_end (ap);
}

void
bar2 (int n, ...)
{
  union U u;
  int j;
  va_list ap;

  va_start (ap, n);
  for (j = 0; j < n; j++)
    {
      b[j] = va_arg (ap, double);
      a[j] = va_arg (ap, DTYPE);
    }
  va_end (ap);
}

void
doit ()
{
  DTYPE x, y, z;
  union U u1, u2;

  /* Sanity check that test setup is right, especially for long double
     which can be changed by command line option.  */
  if (INTS * 4 != sizeof (DTYPE))
    {
#ifdef DBG
      printf ("test error: INTS = %d, sizeof (DTYPE) =  %d\n",
	      INTS, sizeof (DTYPE));
#endif
      __builtin_abort ();
    }

  x = ONE / THREE;
  y = ONE / SEVEN;
  z = ONE / ELEVEN;

  bar0 (1, x);
  u1.d = x; u2.d = a[0]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);

  bar0 (2, x, y);
  u1.d = x; u2.d = a[0]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);

  bar0 (3, x, y, z);
  u1.d = x; u2.d = a[0]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);
  u1.d = z; u2.d = a[2]; compare (0.0, 0.0, u1.i, u2.i, INTS, __LINE__);

  bar1 (1, x, 1.5);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);

  bar1 (2, x, 1.5, y, 2.5);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (2.5, b[1], u1.i, u2.i, INTS, __LINE__);

  bar1 (3, x, 1.5, y, 2.5, z, 3.5);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (2.5, b[1], u1.i, u2.i, INTS, __LINE__);
  u1.d = z; u2.d = a[2]; compare (3.5, b[2], u1.i, u2.i, INTS, __LINE__);

  bar2 (1, 1.5, x);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);

  bar2 (2, 1.5, x, 2.5, y);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (2.5, b[1], u1.i, u2.i, INTS, __LINE__);

  bar2 (3, 1.5, x, 2.5, y, 3.5, z);
  u1.d = x; u2.d = a[0]; compare (1.5, b[0], u1.i, u2.i, INTS, __LINE__);
  u1.d = y; u2.d = a[1]; compare (2.5, b[1], u1.i, u2.i, INTS, __LINE__);
  u1.d = z; u2.d = a[2]; compare (3.5, b[2], u1.i, u2.i, INTS, __LINE__);
}
