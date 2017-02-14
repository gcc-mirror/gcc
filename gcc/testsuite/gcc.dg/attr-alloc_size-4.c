/* PR c/77531 - __attribute__((alloc_size(1,2))) could also warn on
   multiplication overflow
   PR c/78284 - warn on malloc with very large arguments
   Test exercising the ability to detect and diagnose calls to allocation
   functions decorated with attribute alloc_size that either overflow or
   exceed the maximum object size specified by -Walloc-size-larger-than.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Walloc-size-larger-than=1234" } */

#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)
#define UINT_MAX   (INT_MAX * 2U + 1)

#define SIZE_MAX   __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

#define ALLOC_SIZE(...) __attribute__ ((alloc_size (__VA_ARGS__)))

void* f_uint_1 (unsigned) ALLOC_SIZE (1);
void* f_uint_2 (unsigned, unsigned) ALLOC_SIZE (1, 2);
void* f_int_1 (int) ALLOC_SIZE (1);
void* f_int_2 (int, int) ALLOC_SIZE (1, 2);

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

int
signed_range (int min, int max)
{
  extern int random_signed_value (void);
  int val = random_signed_value ();
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

int
signed_anti_range (int min, int max)
{
  extern int random_signed_value (void);
  int val = random_signed_value ();
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
test_uint_cst (void)
{
  const unsigned max = UINT_MAX;

  sink (f_uint_1 (0));
  sink (f_uint_1 (1));
  sink (f_uint_1 (1233));
  sink (f_uint_1 (1234));
  sink (f_uint_1 (1235));       /* { dg-warning "argument 1 value .1235u?. exceeds maximum object size 1234" } */
  sink (f_uint_1 (max - 1));    /* { dg-warning "argument 1 value .\[0-9\]+u?. exceeds maximum object size 1234" } */
  sink (f_uint_1 (max));        /* { dg-warning "argument 1 value .\[0-9\]+u?. exceeds maximum object size 1234" } */
}

void
test_uint_range (unsigned n)
{
  const unsigned max = UINT_MAX;

  sink (f_uint_1 (n));
  sink (f_uint_1 (UR (0, 1)));
  sink (f_uint_1 (UR (0, 1233)));
  sink (f_uint_1 (UR (0, 1234)));
  sink (f_uint_1 (UR (0, 1235)));
  sink (f_uint_1 (UR (1, 1235)));
  sink (f_uint_1 (UR (1234, 1235)));
  sink (f_uint_1 (UR (1235, 1236)));   /* { dg-warning "argument 1 range \\\[\[0-9\]+u?, \[0-9\]+u?\\\] exceeds maximum object size 1234" } */
  sink (f_uint_1 (UR (1, max - 1)));
  sink (f_uint_1 (UR (1, max)));
}

void
test_int_cst (void)
{
  const int min = INT_MIN;
  const int max = INT_MAX;

  sink (f_int_1 (min));   /* { dg-warning "argument 1 value .-\[0-9\]+. is negative" } */
  sink (f_int_1 (-1));    /* { dg-warning "argument 1 value .-1. is negative" } */
  sink (f_int_1 (0));
  sink (f_int_1 (1));
  sink (f_int_1 (1233));
  sink (f_int_1 (1234));
  sink (f_int_1 (max));   /* { dg-warning "argument 1 value .\[0-9\]+u?. exceeds maximum object size 1234" } */
}

void
test_int_range (int n)
{
  const int min = INT_MIN;
  const int max = INT_MAX;

  sink (f_int_1 (n));

  sink (f_int_1 (SR (min, 1234)));
  sink (f_int_1 (SR (-2, -1)));   /* { dg-warning "argument 1 range \\\[-2, -1\\\] is negative" } */

  sink (f_int_1 (SR (1235, 2345)));  /* { dg-warning "argument 1 range \\\[1235, 2345\\\] exceeds maximum object size 1234" } */
  sink (f_int_1 (SR (max - 1, max)));   /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size 1234" } */

  sink (f_int_1 (SAR (-1, 1)));
  sink (f_int_1 (SAR (-2, 12)));
  sink (f_int_1 (SAR (-3, 123)));
  sink (f_int_1 (SAR (-4, 1234)));   /* { dg-warning "argument 1 range \\\[1235, \[0-9\]+\\\] exceeds maximum object size 1234" } */
  sink (f_int_1 (SAR (min + 1, 1233)));

#if __i386__ || __x86_64__
  /* Avoid failures described in bug 79051.  */
  sink (f_int_1 (SAR (min + 2, 1235)));   /* { dg-warning "argument 1 range \\\[1236, \[0-9\]+\\\] exceeds maximum object size 1234" "" { target { i?86-*-* x86_64-*-* } } } */
#endif

  sink (f_int_1 (SAR (0, max)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]*, -1\\\] is negative" } */
  /* The range below includes zero which would be diagnosed by
     -Walloc-size-zero but since all other values are negative it
     is diagnosed by -Walloc-size-larger-than.  */
  sink (f_int_1 (SAR (1, max)));   /* { dg-warning "argument 1 range \\\[-\[0-9\]*, 0\\\] is negative" } */
  sink (f_int_1 (SAR (2, max)));
}

void
test_size_cst (void)
{
  const size_t max = __SIZE_MAX__;

  sink (f_size_1 (0));
  sink (f_size_1 (1));

  sink (f_size_2 (   0, 1234));
  sink (f_size_2 (   1, 1234));
  sink (f_size_2 (   2, 1234));  /* { dg-warning "product .2 \\* 1234. of arguments 1 and 2 exceeds maximum object size \[0-9\]+" } */
  sink (f_size_2 (1234, 1234));  /* { dg-warning "product .1234 \\* 1234. of arguments 1 and 2 exceeds maximum object size 1234" } */
  sink (f_size_2 (1235, 1234));  /* { dg-warning "argument 1 value .1235. exceeds maximum object size 1234" } */
  sink (f_size_2 (1234, 1235));  /* { dg-warning "argument 2 value .1235. exceeds maximum object size 1234" } */
  sink (f_size_2 (1234, max));  /* { dg-warning "argument 2 value .\[0-9\]+. exceeds maximum object size 1234" } */
  sink (f_size_2 (max, 1234));  /* { dg-warning "argument 1 value .\[0-9\]+. exceeds maximum object size 1234" } */
}

void
test_size_range (size_t n)
{
  const size_t max = __SIZE_MAX__;

  sink (f_size_1 (n));

  sink (f_size_1 (UR (0, 1)));
  sink (f_size_1 (UR (0, max - 1)));
  sink (f_size_1 (UR (1, max - 1)));
  sink (f_size_1 (UR (1, max)));

  sink (f_size_1 (UAR (1, 1)));
  /* Since the only valid argument in the anti-range below is zero
     a warning is expected even though -Walloc-zero is not specified.  */
  sink (f_size_1 (UAR (1, 1234)));   /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
  /* The only valid argument in this range is 1.  */
  sink (f_size_1 (UAR (2, max / 2)));

  sink (f_size_2 (n, n));
  sink (f_size_2 (n, 1234));
  sink (f_size_2 (1234, n));

  sink (f_size_2 (UR (0, 1), 1234));
  sink (f_size_2 (UR (0, 1), 1235));   /* { dg-warning "argument 2 value .1235. exceeds maximum object size 1234" } */

  sink (f_size_2 (UR (1235, 1236), n));  /* { dg-warning "argument 1 range \\\[1235, 1236\\\] exceeds maximum object size 1234" } */

  sink (f_size_2 (UR (1235, 1236), UR (max / 2, max)));  /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " } */
/* { dg-warning "argument 2 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size " "argument 2" { target *-*-* } .-1 } */

}
