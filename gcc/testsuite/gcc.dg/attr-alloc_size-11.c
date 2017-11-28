/* Verify that -Walloc-size-greater-than doesn't cause false positives
   for anti-ranges.  Note that not all of the statements below result
   in the argument being represented as an anti-range.

   { dg-do compile }
   { dg-options "-O2 -Walloc-size-larger-than=12 -ftrack-macro-expansion=0" } */

#define SCHAR_MAX __SCHAR_MAX__
#define SCHAR_MIN (-SCHAR_MAX - 1)
#define UCHAR_MAX (SCHAR_MAX * 2 + 1)

#define SHRT_MAX  __SHRT_MAX__
#define SHRT_MIN  (-SHRT_MAX - 1)
#define USHRT_MAX (SHRT_MAX * 2 + 1)

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)
#define UINT_MAX  (INT_MAX * 2U + 1)

#define LONG_MAX __LONG_MAX__
#define LONG_MIN (-LONG_MAX - 1)
#define ULONG_MAX (LONG_MAX * 2LU + 1)

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define PTRDIFF_MIN (-PTRDIFF_MAX - 1)
#define SIZE_MAX    __SIZE_MAX__

#define ALLOC_MAX   12

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

#define CONCAT(a, b)  a ## b
#define CAT(a, b)     CONCAT (a, b)

/* Macro to generate a unique function to test the anti-range
   ~[MIN, MAX] for type T.  */
#define TEST(T, min, max)					\
  void* CAT (test_anti_range_, __LINE__)(T n)			\
  {								\
    extern void* CAT (alloc_anti_range_, __LINE__)(T)		\
      __attribute__ ((alloc_size (1)));				\
    if (min <= n && n <= max)					\
      n = min - 1;						\
    return CAT (alloc_anti_range_, __LINE__)(n);		\
  } typedef void dummy   /* Require a semicolon.  */

/* The following tests fail because of missing range information.  The xfail
   exclusions are PR79356.  */
TEST (signed char, SCHAR_MIN + 2, ALLOC_MAX);   /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" "missing range info for signed char" { xfail { ! { aarch64*-*-* arm*-*-* alpha*-*-* ia64-*-* mips*-*-* powerpc*-*-* sparc*-*-* s390*-*-* visium-*-* } } } } */
TEST (short, SHRT_MIN + 2, ALLOC_MAX); /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" "missing range info for short" { xfail { ! { aarch64*-*-* arm*-*-* alpha*-*-* ia64-*-* mips*-*-* powerpc*-*-* sparc*-*-* s390x-*-* visium-*-* } } } } */
TEST (int, INT_MIN + 2, ALLOC_MAX);    /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int, -3, ALLOC_MAX);             /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int, -2, ALLOC_MAX);             /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int, -1, ALLOC_MAX);             /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int,  0, ALLOC_MAX);             /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int,  1, ALLOC_MAX);             /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (int,  1, INT_MAX - 1);           /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size 12" } */

/* The following two aren't necessarily anti-ranges.  */
TEST (int,  1, INT_MAX);               /* { dg-warning "argument 1 range \\\[-\[0-9\]+, 0\\\] is negative" } */
TEST (int,  0, INT_MAX);               /* { dg-warning "argument 1 range \\\[-\[0-9\]+, -1\\\] is negative" } */

TEST (long, LONG_MIN + 2, ALLOC_MAX);  /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (ptrdiff_t, PTRDIFF_MIN + 2, ALLOC_MAX);  /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */

TEST (unsigned, 0, ALLOC_MAX);         /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (unsigned long, 0, ALLOC_MAX);    /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
TEST (size_t, 0, ALLOC_MAX);           /* { dg-warning "argument 1 range \\\[13, \[0-9\]+\\\] exceeds maximum object size 12" } */
