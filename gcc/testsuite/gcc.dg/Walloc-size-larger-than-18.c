/* PR tree-optimization/88372 - alloc_size attribute is ignored
   on function pointers
   Verify that calls via function pointers declared alloc_size
   with zero or excessive size trigger either -Walloc-zero or
   -Walloc-size-larger-than warnings.
   { dg-do compile }
   { dg-require-effective-target indirect_calls }
   { dg-options "-O2 -Wall -Walloc-zero -ftrack-macro-expansion=0" } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

typedef __SIZE_TYPE__ size_t;


void sink (void*);

#define T(call) sink (call)

ATTR (alloc_size (1)) void* (*ai1)(int, int);
ATTR (alloc_size (2)) void* (*ai2)(int, int);
ATTR (alloc_size (1, 2)) void* (*ai1_2)(int, int);

ATTR (alloc_size (1)) void* (*asz1)(size_t, size_t);
ATTR (alloc_size (2)) void* (*asz2)(size_t, size_t);
ATTR (alloc_size (1, 2)) void* (*asz1_2)(size_t, size_t);


void test_alloc_ptr_zero (void)
{
  T (asz1 (0, 0));      /* { dg-warning "argument 1 value is zero" } */
  T (asz1 (0, 1));      /* { dg-warning "argument 1 value is zero" } */
  T (asz1 (1, 0));
  T (asz1 (1, 1));

  T (asz2 (0, 0));      /* { dg-warning "argument 2 value is zero" } */
  T (asz2 (0, 1));
  T (asz2 (1, 0));      /* { dg-warning "argument 2 value is zero" } */
  T (asz2 (1, 1));

  T (asz1_2 (0, 0));    /* { dg-warning "argument \[12\] value is zero" } */
  T (asz1_2 (1, 0));    /* { dg-warning "argument 2 value is zero" } */
  T (asz1_2 (0, 1));    /* { dg-warning "argument 1 value is zero" } */
  T (asz1_2 (1, 1));
}


void test_alloc_ptr_negative (int n)
{
  T (ai1 (-1, -1));     /* { dg-warning "argument 1 value .-1. is negative" } */
  T (ai1 (-2,  1));     /* { dg-warning "argument 1 value .-2. is negative" } */
  T (ai1 ( 1, -1));
  T (ai1 ( 1,  1));

  T (ai2 (-1, -3));     /* { dg-warning "argument 2 value .-3. is negative" } */
  T (ai2 (-1,  1));
  T (ai2 ( 1, -4));     /* { dg-warning "argument 2 value .-4. is negative" } */
  T (ai2 ( 1,  1));

  T (ai1_2 (-5, -6));   /* { dg-warning "argument \[12\] value .-\[56\]. is negative" } */
  T (ai1_2 ( 1, -7));   /* { dg-warning "argument 2 value .-7. is negative" } */
  T (ai1_2 (-8,  1));   /* { dg-warning "argument 1 value .-8. is negative" } */
  T (ai1_2 ( 1,  1));

  if (n > -1)
    n = -1;

  /* Also verify a simple range.  */
  T (ai1_2 ( 1,  n));   /* { dg-warning "argument 2 range \\\[-\[0-9\]+, -1] is negative" } */
  T (ai1_2 ( n,  1));   /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -1] is negative" } */
}

void test_alloc_ptr_too_big (void)
{
  size_t x = (__SIZE_MAX__ >> 1) + 1;
  size_t y = __SIZE_MAX__ / 5;

  T (asz1 (x, x));     /* { dg-warning "argument 1 value .\[0-9\]+. exceeds" } */
  T (asz1 (x, 1));     /* { dg-warning "argument 1 value .\[0-9\]+. exceeds" } */
  T (asz1 (1, x));
  T (asz1 (1, 1));

  T (asz2 (x, x));     /* { dg-warning "argument 2 value .\[0-9\]+. exceeds" } */
  T (asz2 (x, 1));
  T (asz2 (1, x));     /* { dg-warning "argument 2 value .\[0-9\]+. exceeds" } */
  T (asz2 (1, 1));

  T (asz1_2 (x, x));   /* { dg-warning "argument \[12\] value .\[0-9\]+. exceeds" } */
  T (asz1_2 (y, 3));   /* { dg-warning "product .\[0-9\]+ \\\* 3. of arguments 1 and 2 exceeds" } */
  T (asz1_2 (y, y));   /* { dg-warning "product .\[0-9\]+ \\\* \[0-9\]+. of arguments 1 and 2 exceeds" } */
  T (asz1_2 (1, x));   /* { dg-warning "argument 2 value .\[0-9\]+. exceeds" } */
  T (asz1_2 (x, 1));   /* { dg-warning "argument 1 value .\[0-9\]+. exceeds" } */
  T (asz1_2 (1, 1));

}
