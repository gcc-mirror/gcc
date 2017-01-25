/* PR c/77531 - __attribute__((alloc_size(1,2))) could also warn on
   multiplication overflow
   PR c/78284 - warn on malloc with very large arguments
   Test exercising the ability to detect and diagnose calls to allocation
   functions decorated with attribute alloc_size that either overflow or
   exceed the default maximum object size (with -Walloc-size-larger-than
   not explicitly specified).  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

#define SCHAR_MAX  __SCHAR_MAX__
#define SCHAR_MIN  (-SCHAR_MAX - 1)
#define UCHAR_MAX  (SCHAR_MAX * 2 + 1)

#define SHRT_MAX   __SHRT_MAX__
#define SHRT_MIN   (-SHRT_MAX - 1)
#define USHRT_MAX  (SHRT_MAX * 2 + 1)

#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)
#define UINT_MAX   (INT_MAX * 2U + 1)

#define LONG_MAX   __LONG_MAX__
#define LONG_MIN   (-LONG_MAX - 1L)
#define ULONG_MAX  (LONG_MAX * 2LU + 1)

#define LLONG_MAX  __LLONG_MAX__
#define LLONG_MIN  (-LLONG_MAX - 1LL)
#define ULLONG_MAX (ULLONG_MAX * 2LLU + 1)

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define PTRDIFF_MIN (-PTRDIFF_MAX - 1)
#define SIZE_MAX    __SIZE_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

#define ALLOC_SIZE(...) __attribute__ ((alloc_size (__VA_ARGS__)))

void* f_uchar_1 (unsigned char) ALLOC_SIZE (1);
void* f_uchar_2 (unsigned char, unsigned char) ALLOC_SIZE (1, 2);
void* f_schar_1 (signed char) ALLOC_SIZE (1);
void* f_schar_2 (signed char, signed char) ALLOC_SIZE (1, 2);

void* f_ushrt_1 (unsigned short) ALLOC_SIZE (1);
void* f_ushrt_2 (unsigned short, unsigned short) ALLOC_SIZE (1, 2);
void* f_shrt_1 (signed short) ALLOC_SIZE (1);
void* f_shrt_2 (signed short, signed short) ALLOC_SIZE (1, 2);

void* f_uint_1 (unsigned) ALLOC_SIZE (1);
void* f_uint_2 (unsigned, unsigned) ALLOC_SIZE (1, 2);
void* f_int_1 (int) ALLOC_SIZE (1);
void* f_int_2 (int, int) ALLOC_SIZE (1, 2);

void* f_ulong_1 (unsigned long) ALLOC_SIZE (1);
void* f_ulong_2 (unsigned long, unsigned long) ALLOC_SIZE (1, 2);
void* f_long_1 (long) ALLOC_SIZE (1);
void* f_long_2 (long, long) ALLOC_SIZE (1, 2);

void* f_ullong_1 (unsigned long long) ALLOC_SIZE (1);
void* f_ullong_2 (unsigned long long, unsigned long long) ALLOC_SIZE (1, 2);
void* f_llong_1 (long long) ALLOC_SIZE (1);
void* f_llong_2 (long long, long long) ALLOC_SIZE (1, 2);

void* f_size_1 (size_t) ALLOC_SIZE (1);
void* f_size_2 (size_t, size_t) ALLOC_SIZE (1, 2);

size_t
unsigned_range (size_t min, size_t max)
{
  extern size_t random_unsigned_value (void);
  size_t val = random_unsigned_value ();
  if (val < min || max < val) val = min;
  return val;
}

long long
signed_range (long long min, long long max)
{
  extern long long random_signed_value (void);
  long long val = random_signed_value ();
  if (val < min || max < val) val = min;
  return val;
}

size_t
unsigned_anti_range (size_t min, size_t max)
{
  extern size_t random_unsigned_value (void);
  size_t val = random_unsigned_value ();
  if (min <= val && val <= max)
    val = min - 1;
  return val;
}

long long
signed_anti_range (long long min, long long max)
{
  extern long long random_signed_value (void);
  long long val = random_signed_value ();
  if (min <= val && val <= max)
    val = min - 1;
  return val;
}

#define UR(min, max) unsigned_range (min, max)
#define SR(min, max) signed_range (min, max)

#define UAR(min, max) unsigned_anti_range (min, max)
#define SAR(min, max) signed_anti_range (min, max)


void sink (void*);

void
test_uchar_cst (void)
{
  const unsigned char max = UCHAR_MAX;

  sink (f_uchar_1 (0));
  sink (f_uchar_1 (1));
  sink (f_uchar_1 (max));

  sink (f_uchar_2 (0, 0));
  sink (f_uchar_2 (0, 1));
  sink (f_uchar_2 (1, 0));
  sink (f_uchar_2 (1, 1));
  sink (f_uchar_2 (0, max));
  sink (f_uchar_2 (max, 0));
  sink (f_uchar_2 (max, max));
}

void
test_uchar_range (unsigned char n, int i)
{
  const unsigned char max = UCHAR_MAX;

  sink (f_uchar_1 (n));

  sink (f_uchar_1 (UR (0, 1)));
  sink (f_uchar_1 (UR (1, max)));
  sink (f_uchar_1 (UR (0, max - 1)));

  sink (f_uchar_1 (UAR (1, 1)));
  sink (f_uchar_1 (UAR (1, max - 1)));
  sink (f_uchar_1 (UAR (max - 2, max - 1)));

  sink (f_uchar_2 (0, n));
  sink (f_uchar_2 (0, i));
  sink (f_uchar_2 (n, 0));
  sink (f_uchar_2 (i, 0));
  sink (f_uchar_2 (1, n));
  sink (f_uchar_2 (1, i));
  sink (f_uchar_2 (n, 1));
  sink (f_uchar_2 (i, 1));
  sink (f_uchar_2 (max, n));
  sink (f_uchar_2 (max, i));
  sink (f_uchar_2 (n, max));
  sink (f_uchar_2 (i, max));
  sink (f_uchar_2 (n, n));
  sink (f_uchar_2 (i, i));

  sink (f_uchar_2 (UR (0, 1), UR (0, 1)));
  sink (f_uchar_2 (UR (1, 2), UR (1, 2)));
  sink (f_uchar_2 (UR (1, max), UR (0, 1)));
  sink (f_uchar_2 (UR (0, 1), UR (1, max)));
}

void
test_schar_cst (void)
{
  const signed char min = SCHAR_MIN;
  const signed char max = SCHAR_MAX;

  sink (f_schar_1 (min));     /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" } */
  sink (f_schar_1 (-1));      /* { dg-warning "argument 1 value .-1. is negative" } */
  sink (f_schar_1 (0));
  sink (f_schar_1 (1));
  sink (f_schar_1 (max));

  sink (f_schar_2 (0, min));     /* { dg-warning "argument 2 value .-\[0-9\]+. is negative" } */
  sink (f_schar_2 (min, 0));     /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" } */
  sink (f_schar_2 (0, -1));      /* { dg-warning "argument 2 value .-1. is negative" } */
  sink (f_schar_2 (-1, 0));      /* { dg-warning "argument 1 value .-1. is negative" } */

}

void
test_schar_range (signed char n)
{
  const signed char min = SCHAR_MIN;
  const signed char max = SCHAR_MAX;

  sink (f_schar_1 (n));

  sink (f_schar_1 (SR (min, min + 1)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_schar_1 (SR (min, 0)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_1 (SR (-1, 0)));   /* { dg-warning "argument 1 range \\\[-1, 0\\\] is negative" } */
  sink (f_schar_1 (SR (-1, 1)));
  sink (f_schar_1 (SR (0, 1)));
  sink (f_schar_1 (SR (0, max - 1)));
  sink (f_schar_1 (SR (1, max)));
  sink (f_schar_1 (SR (max - 1, max)));

  sink (f_schar_2 (n, n));

  sink (f_schar_2 (SR (min, min + 1), n));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_schar_2 (n, SR (min, min + 1)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_schar_2 (SR (min, min + 1), 0));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_schar_2 (0, SR (min, min + 1)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_schar_2 (SR (min, min + 1), min));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  /* { dg-warning "argument 2 value .-\[0-9\]+. is negative" "argument 2" { target *-*-* } .-1 } */
  sink (f_schar_2 (min, SR (min, min + 1)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" "argument 1" { target *-*-* } .-1 } */

  sink (f_schar_2 (SR (-1, 0), 0));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_2 (0, SR (-1, 0)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_2 (SR (-1, 0), 1));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_2 (1, SR (-1, 0)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_2 (SR (-1, 0), n));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_schar_2 (n, SR (-1, 0)));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, 0\\\] is negative" } */

  sink (f_schar_2 (max, SR (1, max)));
  sink (f_schar_2 (SR (1, max), max));
}

void
test_ushrt_cst (void)
{
  const unsigned short max = USHRT_MAX;

  sink (f_ushrt_1 (0));
  sink (f_ushrt_1 (1));
  sink (f_ushrt_1 (max));

  sink (f_ushrt_2 (0, 0));
  sink (f_ushrt_2 (0, 1));
  sink (f_ushrt_2 (1, 0));
  sink (f_ushrt_2 (1, 1));
  sink (f_ushrt_2 (0, max));
  sink (f_ushrt_2 (max, 0));

  if (max < SIZE_MAX && (size_t)max * max < SIZE_MAX / 2)
    sink (f_ushrt_2 (max, max));
}

void
test_ushrt_range (unsigned short n)
{
  const unsigned short max = USHRT_MAX;

  sink (f_ushrt_1 (n));
  sink (f_ushrt_1 (UR (0, 1)));
  sink (f_ushrt_1 (UR (1, max - 1)));
  sink (f_ushrt_1 (UR (1, max)));
  sink (f_ushrt_1 (UR (0, max - 1)));
}

void
test_shrt_cst (void)
{
  const short min = SHRT_MIN;
  const short max = SHRT_MAX;

  sink (f_shrt_1 (min));   /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" } */
  sink (f_shrt_1 (-1));         /* { dg-warning "argument 1 value .-1. is negative" } */
  sink (f_shrt_1 (0));
  sink (f_shrt_1 (1));
  sink (f_shrt_1 (max));
}

void
test_shrt_range (short n)
{
  const short min = SHRT_MIN;
  const short max = SHRT_MAX;

  sink (f_shrt_1 (n));

  sink (f_shrt_1 (SR (min, min + 1)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_shrt_1 (SR (min, 0)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_shrt_1 (SR (-1, 0)));   /* { dg-warning "argument 1 range \\\[-1, 0\\\] is negative" } */
  sink (f_shrt_1 (SR (-1, 1)));
  sink (f_shrt_1 (SR (0, 1)));
  sink (f_shrt_1 (SR (0, max - 1)));
  sink (f_shrt_1 (SR (1, max)));
  sink (f_shrt_1 (SR (max - 1, max)));
}

void
test_uint_cst (void)
{
  const unsigned max = UINT_MAX;

  sink (f_uint_1 (0));
  sink (f_uint_1 (1));

  if (max < SIZE_MAX)
    {
      sink (f_uint_1 (max - 1));
      sink (f_uint_1 (max));
    }
}

void
test_uint_range (unsigned n)
{
  const unsigned max = UINT_MAX;

  sink (f_uint_1 (n));
  sink (f_uint_1 (UR (0, 1)));
  sink (f_uint_1 (UR (0, max - 1)));
  sink (f_uint_1 (UR (1, max - 1)));
  sink (f_uint_1 (UR (1, max)));
}

void
test_int_cst (void)
{
  const int min = INT_MIN;
  const int max = INT_MAX;

  sink (f_int_1 (min));   /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" } */
  sink (f_int_1 (-1));         /* { dg-warning "argument 1 value .-1. is negative" } */
  sink (f_int_1 (0));
  sink (f_int_1 (1));
  sink (f_int_1 (max));
}

void
test_int_range (int n)
{
  const int min = INT_MIN;
  const int max = INT_MAX;

  sink (f_int_1 (n));

  sink (f_int_1 (SR (min, min + 1)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -\[0-9\]+\\\] is negative" } */
  sink (f_int_1 (SR (min, 0)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
  sink (f_int_1 (SR (-1, 0)));   /* { dg-warning "argument 1 range \\\[-1, 0\\\] is negative" } */
  sink (f_int_1 (SR (-1, 1)));
  sink (f_int_1 (SR (0, 1)));
  sink (f_int_1 (SR (0, max - 1)));
  sink (f_int_1 (SR (1, max)));
  sink (f_int_1 (SR (max - 1, max)));
}

void
test_ulong_cst (void)
{
  const unsigned long max = ULONG_MAX;

  sink (f_ulong_1 (0));
  sink (f_ulong_1 (1));
#if ULONG_MAX < SIZE_MAX
  sink (f_ulong_1 (max - 1));
  sink (f_ulong_1 (max));
#else
  (void)&max;
#endif
}

void
test_ulong_range (unsigned long n)
{
  const unsigned long max = ULONG_MAX;

  sink (f_ulong_1 (n));
  sink (f_ulong_1 (UR (0, 1)));
  sink (f_ulong_1 (UR (0, max - 1)));
  sink (f_ulong_1 (UR (1, max - 1)));
  sink (f_ulong_1 (UR (1, max)));
}

void
test_long_cst (void)
{
  const long min = LONG_MIN;
  const long max = LONG_MAX;

  sink (f_long_1 (min));   /* { dg-warning "argument 1 value .-\[0-9\]+l*. is negative" } */
  sink (f_long_1 (-1));         /* { dg-warning "argument 1 value .-1l*. is negative" } */
  sink (f_long_1 (0));
  sink (f_long_1 (1));
  sink (f_long_1 (max));
}

void
test_long_range (long n)
{
  const long min = LONG_MIN;
  const long max = LONG_MAX;

  sink (f_long_1 (n));

  sink (f_long_1 (SR (min, min + 1)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+l*, -\[0-9\]+l*\\\] is negative" } */
  sink (f_long_1 (SR (min, 0)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+l*, 0l*\\\] is negative" } */
  sink (f_long_1 (SR (-1, 0)));   /* { dg-warning "argument 1 range \\\[-1l*, 0l*\\\] is negative" } */
  sink (f_long_1 (SR (-1, 1)));
  sink (f_long_1 (SR (0, 1)));
  sink (f_long_1 (SR (0, max - 1)));
  sink (f_long_1 (SR (1, max)));
  sink (f_long_1 (SR (max - 1, max)));
}

void
test_size_cst (void)
{
  const size_t max = __SIZE_MAX__;

  sink (f_size_1 (0));
  sink (f_size_1 (1));
  sink (f_size_1 (max - 1));  /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_1 (max));      /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */

  sink (f_size_2 (0, max - 1));  /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (max - 1, 0));  /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (1, max - 1));  /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (max - 1, 1));  /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (max - 1, max - 1));  /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" "argument 2" { target *-*-* } .-1 } */

  sink (f_size_2 (0, max));      /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (max, 0));      /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size \[0-9\]+" } */

  sink (f_size_2 (max / 2, 2));      /* { dg-warning "product .\[0-9\]+ \\* \[0-9\]+. of arguments 1 and 2 exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (max / 2, 3));      /* { dg-warning "product .\[0-9\]+ \\* \[0-9\]+. of arguments 1 and 2 exceeds .SIZE_MAX." } */
}

void
test_size_range (size_t ui, ptrdiff_t si)
{
  const ptrdiff_t smin = PTRDIFF_MIN;
  const ptrdiff_t smax = PTRDIFF_MAX;
  const size_t umax = SIZE_MAX;

  sink (f_size_1 (ui));
  sink (f_size_1 (si));

  sink (f_size_1 (UR (0, 1)));
  sink (f_size_1 (UR (0, umax - 1)));
  sink (f_size_1 (UR (1, umax - 1)));
  sink (f_size_1 (UR (1, umax)));

  sink (f_size_1 (UAR (1, 1)));
  /* Since the only valid argument in the anti-range below is zero
     a warning is expected even though -Walloc-zero is not specified.  */
  sink (f_size_1 (UAR (1, umax / 2)));   /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
  /* The only valid argument in this range is 1.  */
  sink (f_size_1 (UAR (2, umax / 2)));

  sink (f_size_2 (ui, ui));
  sink (f_size_2 (si, si));
  sink (f_size_2 (ui, umax / 2));
  sink (f_size_2 (si, umax / 2));
  sink (f_size_2 (umax / 2, ui));
  sink (f_size_2 (umax / 2, si));

  sink (f_size_2 (UR (0, 1), umax));   /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size " } */
  sink (f_size_2 (UR (0, 1), umax / 2));
  sink (f_size_2 (UR (0, umax / 2), umax / 2));

  sink (f_size_2 (UR (umax / 2 + 1, umax / 2 + 2), ui));  /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
  sink (f_size_2 (ui, UR (umax / 2 + 1, umax / 2 + 2)));  /* { dg-warning "argument 2 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
  sink (f_size_2 (UR (umax / 2 + 1, umax), UR (umax / 2 + 1, umax)));  /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
/* { dg-warning "argument 2 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " "argument 2" { target *-*-* } .-1 } */

  sink (f_size_2 (SR (smin, 1), 1));
  sink (f_size_2 (SR (smin, 1), umax / 2));
  sink (f_size_2 (SR (-1, smax), 1));
  sink (f_size_2 (SR (-1, smax), umax / 2));
  sink (f_size_2 (SR (-1, 1), 1));
  sink (f_size_2 (SR (-1, 1), umax / 2));
  sink (f_size_2 (SR (-9, 9), 1));
  sink (f_size_2 (SR (-9, 9), umax / 2));
}
