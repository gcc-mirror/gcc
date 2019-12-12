/* PR tree-optimization/80669 - Bad -Wstringop-overflow warnings for stpncpy
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -Wno-restrict -Wno-stringop-truncation" } */

#define SIZE_MAX __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

void sink (char*);

#define stpncpy (d, s, n)  sink (__builtin_stpncpy (d, s, n))

size_t value (void);

static size_t range (size_t min, size_t max)
{
  size_t val = value ();
  return val < min || max < val ? min : val;
}

/* Verify that no -Wstringop-overflow warning is issued for stpncpy
   with constant size.  (Some tests cause -Wstringop-truncation and
   that's expected).  */
void test_cst (char *d)
{
  __builtin_stpncpy (d, "123", 0);
  __builtin_stpncpy (d, "123", 1);
  __builtin_stpncpy (d, "123", 2);
  __builtin_stpncpy (d, "123", 3);
  __builtin_stpncpy (d, "123", 4);
  __builtin_stpncpy (d, "123", 5);
  __builtin_stpncpy (d, "123", 999);

  size_t n = SIZE_MAX / 2;

  __builtin_stpncpy (d, "123", n);

  __builtin_stpncpy (d, "123", n + 1);    /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */
}


/* Verify that no -Wstringop-overflow warning is issued for stpncpy
   with size in some range.  */
void test_rng (char *d)
{
#define R(min, max) range (min, max)

  __builtin_stpncpy (d, "123", R (0, 1));
  __builtin_stpncpy (d, "123", R (0, 2));
  __builtin_stpncpy (d, "123", R (0, 3));
  __builtin_stpncpy (d, "123", R (0, 4));
  __builtin_stpncpy (d, "123", R (0, 5));

  __builtin_stpncpy (d, "123", R (1, 2));
  __builtin_stpncpy (d, "123", R (1, 3));
  __builtin_stpncpy (d, "123", R (1, 4));
  __builtin_stpncpy (d, "123", R (1, 5));

  __builtin_stpncpy (d, "123", R (2, 3));
  __builtin_stpncpy (d, "123", R (2, 4));
  __builtin_stpncpy (d, "123", R (2, 5));

  __builtin_stpncpy (d, "123", R (3, 4));
  __builtin_stpncpy (d, "123", R (3, 5));

  __builtin_stpncpy (d, "123", R (4, 5));

  __builtin_stpncpy (d, "123", R (5, 6));

  __builtin_stpncpy (d, "123", R (12345, 23456));

  size_t n = SIZE_MAX / 2;

  __builtin_stpncpy (d, "123", R (n - 1, n + 1));

  __builtin_stpncpy (d, "123", R (n + 1, n + 2));   /* { dg-warning "specified bound between \[0-9\]+ and \[0-9\]+ exceeds maximum object size \[0-9\]+" } */
}
