/* Test that the preprocessor is capable of 64-bit arithmetic.
   (Must turn off -pedantic, since `LL' constants are only in C9x.)  */
/* { dg-do preprocess } */
/* { dg-options "" } */

#define U_MAX 4294967295U
#define ULL_MAX 18446744073709551615ULL
#define LL_MAX 9223372036854775807LL
#define LL_MIN (-LL_MAX-1)

/* Check simple truncation. */
#if U_MAX == ULL_MAX || LL_MIN == 0 || LL_MAX == -1
#error "simple truncation"  /* { dg-bogus "trunc" "simple truncation" } */
#endif

/* Check left/right shifting with all bits set and with one bit set. */
#if !(~0ULL >> 63) || !(~0ULL << 63) || !(~0LL >> 63) || !(~0LL << 63) || \
  !(LL_MIN >> 63) || !(1LL << 62) || !(ULL_MAX >> 63) || !(1ULL << 63)
#error "bit shift truncation" /* { dg-bogus "trunc" "bit shift truncation" } */
#endif

/* Check math expressions. */
#if (2ULL * U_MAX < U_MAX) || (1ULL + U_MAX < U_MAX)
#error "math truncation"  /* { dg-bogus "trunc" "math truncation" } */
#endif
