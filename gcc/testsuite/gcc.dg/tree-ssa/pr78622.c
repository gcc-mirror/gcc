/* PR middle-end/78622 - [7 Regression] -Wformat-overflow/-fprintf-return-value
   incorrect with overflow/wrapping
   { dg-do compile }
   { dg-options "-Wformat-overflow=2" }
   The h and hh length modifiers are a C99 feature (see PR 78959).
   { dg-require-effective-target c99_runtime }  */

char buf[1];

int test_uchar_hhd (unsigned char x)
{
  if (x < 64 || x > 2U * __SCHAR_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%hhd", x + 1);   /* { dg-warning "directive writing between 1 and 4 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}

int test_uint_hhd (unsigned x)
{
  if (x < 64 || x > 2U * __INT_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%hhd", x + 1);   /* { dg-warning "directive writing between 1 and 4 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}

int test_schar_hhu (signed char x)
{
  if (x < -9 || x > 9)
    return -1;

  return __builtin_sprintf (buf, "%hhu", x + 1);   /* { dg-warning "directive writing between 1 and 3 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}

int test_ushort_hd (unsigned short x)
{
  if (x < 64 || x > 2U * __SHRT_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%hd", x + 1);   /* { dg-warning "directive writing between 1 and 6 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}

int test_uint_d (unsigned x)
{
  if (x < 64 || x > 2U * __INT_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%d", x + 1);   /* { dg-warning "directive writing between 1 and 11 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}

int test_ulong_ld (unsigned long x)
{
  if (x < 64 || x > 2LU * __LONG_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%ld", x + 1);   /* { dg-warning "directive writing between 1 and 11 bytes into a region of size 1" "ilp32" { target { ilp32 } } } */
  /* { dg-warning "directive writing between 1 and 20 bytes into a region of size 1" "lp64" { target { lp64 } } .-1 } */
}

int test_ullong_lld (unsigned long long x)
{
  if (x < 64 || x > 2LLU * __LONG_LONG_MAX__ - 10)
    return -1;

  return __builtin_sprintf (buf, "%lld", x + 1);   /* { dg-warning "directive writing between 1 and 20 bytes into a region of size 1" "int32plus" { target { int32plus } } } */
}
