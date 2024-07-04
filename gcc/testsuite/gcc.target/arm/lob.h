#include <string.h>
#include <stdint.h>
/* Common code for lob tests.  */

#define NO_LOB asm volatile ("@ clobber lr" : : : "lr" )

#define N 100

static void
reset_data (int *a, int *b, int *c, int x)
{
  memset (a, -1, x * sizeof (*a));
  memset (b, -1, x * sizeof (*b));
  memset (c, 0, x * sizeof (*c));
}

static void
reset_data8 (int8_t *a, int8_t *b, int8_t *c, int x)
{
  memset (a, -1, x * sizeof (*a));
  memset (b, -1, x * sizeof (*b));
  memset (c, 0, x * sizeof (*c));
}

static void
reset_data16 (int16_t *a, int16_t *b, int16_t *c, int x)
{
  memset (a, -1, x * sizeof (*a));
  memset (b, -1, x * sizeof (*b));
  memset (c, 0, x * sizeof (*c));
}

static void
reset_data32 (int32_t *a, int32_t *b, int32_t *c, int x)
{
  memset (a, -1, x * sizeof (*a));
  memset (b, -1, x * sizeof (*b));
  memset (c, 0, x * sizeof (*c));
}

static void
reset_data64 (int64_t *a, int64_t *c, int x)
{
  memset (a, -1, x * sizeof (*a));
  memset (c, 0, x * sizeof (*c));
}

static void
check_plus (int *a, int *b, int *c, int x)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (i < x)
	{
	  if (c[i] != (a[i] + b[i])) abort ();
	}
      else
	{
	  if (c[i] != 0) abort ();
	}
    }
}

static void
check_plus8 (int8_t *a, int8_t *b, int8_t *c, int x)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (i < x)
	{
	  if (c[i] != (a[i] + b[i])) abort ();
	}
      else
	{
	  if (c[i] != 0) abort ();
	}
    }
}

static void
check_plus16 (int16_t *a, int16_t *b, int16_t *c, int x)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (i < x)
	{
	  if (c[i] != (a[i] + b[i])) abort ();
	}
      else
	{
	  if (c[i] != 0) abort ();
	}
    }
}

static void
check_plus32 (int32_t *a, int32_t *b, int32_t *c, int x)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (i < x)
	{
	  if (c[i] != (a[i] + b[i])) abort ();
	}
      else
	{
	  if (c[i] != 0) abort ();
	}
    }
}

static void
check_memcpy64 (int64_t *a, int64_t *c, int x)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (i < x)
	{
	  if (c[i] != a[i]) abort ();
	}
      else
	{
	  if (c[i] != 0) abort ();
	}
    }
}
