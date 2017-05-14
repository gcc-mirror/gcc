/* PR tree-optimization/77671 - missing -Wformat-overflow warning
   on sprintf overflow with "%s"

   Negative test verifying that sprintf family calls that must not
   be transformed into calls to other functions (such as memcpy)
   are preserved.

   { dg-compile }
   { dg-options "-O2 -Wformat -Wno-format-truncation -Wno-format-zero-length -fdump-tree-optimized" } */

void sink (char*, ...);

extern char buffer[];

/* String exactly 4100 characters long (plus the terminating NUL).  */
extern const char s4100[4101];

void test_sprintf (const char *s)
{
  /* Macros to test the function call while eignoring and using
     the return value, respectively.  */
#define IGN(...) __builtin_sprintf (buffer, __VA_ARGS__), sink (buffer)
#define USE(...) sink (buffer, __builtin_sprintf (buffer, __VA_ARGS__))

  /* All of the following calls to sprintf must be emitted (and not
     transformed into memcpy, strcpy, or similar).  */

  /* Verify that a sprintf call with output in excess of the maximum
     of 4095 bytes is not transformed into memcpy/strcpy when its
     return value is used (the call may fail with EOVERFLOW but
     the error is only detectable when the function's negative return
     value indicates errno is valid ).  */
  USE (s4100);

  USE ("%s", s4100);

  /* Same as above but with string of unknown length (which could
     be in excess of 4K long).  */
  USE (s);
  USE ("%s", s);
}


void test_snprintf (void)
{
#undef IGN
#define IGN(N, ...) __builtin_snprintf (buffer, N, __VA_ARGS__); sink (buffer)

  /* All of the following calls to sprintf must be emitted (and not
     transformed into memcpy, strcpy, or similar).  */

  /* Truncated output could be optimized into strncpy (not done yet).  */
  IGN (1, "123");
  IGN (1, s4100);

  IGN (1, "%s", "123");
  IGN (1, "%s", s4100);

  /* Output in excess of the maximum of 4095 bytes.  */
  IGN (4097, s4100);

  IGN (4097, "%s", s4100);
}


void test_vsprintf (__builtin_va_list va)
{
#undef IGN
#define IGN(fmt) __builtin_vsprintf (buffer, fmt, va); sink (buffer)

  /* All of the following calls to vsprintf must be emitted (and not
     transformed into memcpy, strcpy, or similar).  */

  /* Output in excess of the maximum of 4095 bytes.  */
  IGN (s4100);
}


void test_vsnprintf (__builtin_va_list va)
{
#undef IGN
#define IGN(N, fmt) __builtin_vsnprintf (buffer, N, fmt, va); sink (buffer)

  /* All of the following calls to vsnprintf must be emitted (and not
     transformed into memcpy, strcpy, or similar).  */

  /* Truncated output could be optimized into strncpy (not done yet).  */
  IGN (1, "123");
  IGN (1, s4100);

  /* Output in excess of the maximum of 4095 bytes.  */
  IGN (4097, s4100);
}

/* { dg-final { scan-tree-dump-times "builtin_sprintf" 4 "optimized" } }
   { dg-final { scan-tree-dump-times "builtin_snprintf" 6 "optimized" } }
   { dg-final { scan-tree-dump-times "builtin_vsprintf" 1 "optimized" } }
   { dg-final { scan-tree-dump-times "builtin_vsnprintf" 3 "optimized" } } */

#define S10    "0123456789"
#define S100   S10 S10 S10 S10 S10  S10 S10 S10 S10 S10
#define S1000  S100 S100 S100 S100 S100  S100 S100 S100 S100 S100

const char s4100[4101] = S1000 S1000 S1000 S1000 S100;
