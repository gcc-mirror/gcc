/* Verify that -Walloc-size-greater-than doesn't cause false positives
   for anti-ranges.  Note that not all of the statements used to create
   anti-ranges below result in the argument being represented as an anti
   range.

   { dg-do compile }
   { dg-options "-O2 -Walloc-size-larger-than=12" } 
   { dg-options "-Wno-overflow" { target { ! int32plus } } } */

#define SCHAR_MAX __SCHAR_MAX__
#define SCHAR_MIN (-SCHAR_MAX - 1)
#define UCHAR_MAX (SCHAR_MAX * 2 + 1)

#define SHRT_MAX  __SHRT_MAX__
#define SHRT_MIN  (-SHRT_MAX - 1)
#define USHRT_MAX (SHRT_MAX * 2U + 1)

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)
#define UINT_MAX  (INT_MAX * 2U + 1)

#define LONG_MAX __LONG_MAX__
#define LONG_MIN (-LONG_MAX - 1)
#define ULONG_MAX (LONG_MAX * 2LU + 1)

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define PTRDIFF_MIN (-PTRDIFF_MAX - 1)
#define SIZE_MAX    __SIZE_MAX__

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


/* Verify the anti-range ~[TYPE_MAX - 1, TYPE_MAX - 1].  */
TEST (signed char, SCHAR_MAX - 1, SCHAR_MAX - 1);
TEST (unsigned char, UCHAR_MAX - 1, UCHAR_MAX - 1);
TEST (short, SHRT_MAX - 1, SHRT_MAX - 1);
TEST (unsigned short, USHRT_MAX - 1, USHRT_MAX - 1);
TEST (int, INT_MAX - 1, INT_MAX - 1);
TEST (unsigned, UINT_MAX - 1, UINT_MAX - 1);
TEST (long, LONG_MAX - 1, LONG_MAX - 1);
TEST (unsigned long, ULONG_MAX - 1, ULONG_MAX - 1);
TEST (ptrdiff_t, PTRDIFF_MAX - 1, PTRDIFF_MAX - 1);
TEST (size_t, SIZE_MAX - 1, SIZE_MAX - 1);

/* Verify ~[0, 0].  */
TEST (signed char, 0, 0);
TEST (unsigned char, 0, 0);
TEST (short, 0, 0);
TEST (unsigned short, 0, 0);
TEST (int, 0, 0);
TEST (unsigned, 0, 0);
TEST (long, 0, 0);
TEST (unsigned long, 0, 0);
TEST (ptrdiff_t, 0, 0);
TEST (size_t, 0, 0);

/* Verify ~[1, 1].  */
TEST (signed char, 1, 1);
TEST (unsigned char, 1, 1);
TEST (short, 1, 1);
TEST (unsigned short, 1, 1);
TEST (int, 1, 1);
TEST (unsigned, 1, 1);
TEST (long, 1, 1);
TEST (unsigned long, 1, 1);
TEST (ptrdiff_t, 1, 1);
TEST (size_t, 1, 1);


/* Verify ~[TYPE_MAX - 2, TYPE_MAX - 1].  */
TEST (signed char, SCHAR_MAX - 2, SCHAR_MAX - 1);
TEST (unsigned char, UCHAR_MAX - 2, UCHAR_MAX - 1);
TEST (short, SHRT_MAX - 2, SHRT_MAX - 1);
TEST (unsigned short, USHRT_MAX - 2, USHRT_MAX - 1);
TEST (int, INT_MAX - 2, INT_MAX - 1);
TEST (unsigned, UINT_MAX - 2, UINT_MAX - 1);
TEST (long, LONG_MAX - 2, LONG_MAX - 1);
TEST (unsigned long, ULONG_MAX - 2, ULONG_MAX - 1);
TEST (ptrdiff_t, PTRDIFF_MAX - 2, PTRDIFF_MAX - 1);
TEST (size_t, SIZE_MAX - 2, SIZE_MAX - 1);

/* Verify ~[0, 2].  */
TEST (signed char, 0, 2);
TEST (unsigned char, 0, 2);
TEST (short, 0, 2);
TEST (unsigned short, 0, 2);
TEST (int, 0, 2);
TEST (unsigned int, 0, 2);
TEST (long, 0, 2);
TEST (unsigned long, 0, 2);
TEST (ptrdiff_t, 0, 2);
TEST (size_t, 0, 2);

/* Verify the signed anti-range ~[TYPE_MIN - 2, -1].  */
TEST (signed char, SCHAR_MIN + 2, -1);
TEST (short, SHRT_MIN + 2, -1);
TEST (int, INT_MIN + 2, -1);
TEST (long, LONG_MIN + 2, -1);
TEST (ptrdiff_t, PTRDIFF_MIN + 2, -1);

/* Verify the signed anti-range ~[TYPE_MIN - 2, 0].  */
TEST (signed char, SCHAR_MIN + 2, 0);
TEST (short, SHRT_MIN + 2, 0);
TEST (int, INT_MIN + 2, 0);
TEST (long, LONG_MIN + 2, 0);
TEST (ptrdiff_t, PTRDIFF_MIN + 2, 0);

/* Verify the signed anti-range ~[TYPE_MIN - 2, 1].  */
TEST (signed char, SCHAR_MIN + 2, 1);
TEST (short, SHRT_MIN + 2, 1);
TEST (int, INT_MIN + 2, 1);
TEST (long, LONG_MIN + 2, 1);
TEST (ptrdiff_t, PTRDIFF_MIN + 2, 1);

/* Verify the signed anti-range ~[TYPE_MIN - 2, 2].  */
TEST (signed char, SCHAR_MIN + 2, 2);
TEST (short, SHRT_MIN + 2, 2);
TEST (int, INT_MIN + 2, 2);
TEST (long, LONG_MIN + 2, 2);
TEST (ptrdiff_t, PTRDIFF_MIN + 2, 2);

/* Verify the signed anti-range ~[-1, 2].  */
TEST (signed char, -1, 2);
TEST (short, -1, 2);
TEST (int, -1, 2);
TEST (long, -1, 2);
TEST (ptrdiff_t, 01, 2);
