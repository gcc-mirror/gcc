/* PR tree-optimization/86853 - sprintf optimization for wide strings
   doesn't account for conversion failureâ€‹
   Exercise wide character handling in an EBCDIC execution charset.
   { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-options "-O2 -Wall -Wno-format -Wformat-overflow -fexec-charset=IBM1047 -fdump-tree-optimized" } */

typedef __WCHAR_TYPE__ wchar_t;

/* Exercise wide character constants. */

void test_lc_cst (void)
{
  /* IBM1047 0x30 maps to ASCII 0x94 which neeed not be representable
     in the current locale (and the snprintf() call may fail).  Verify
     that snprintf() doesn't assume it is.  */
  wchar_t wc = 0x30;

  int n = __builtin_snprintf (0, 0, "%lc", wc);
  if (n < 0)
    __builtin_abort ();
}

void test_C_cst (void)
{
  /* Same as above but for %C and 0x31 which maps to 0x95.  */
  wchar_t wc = 0x31;

  int n = __builtin_snprintf (0, 0, "%C", wc);
  if (n < 0)
    __builtin_abort ();
}

/* Exercise wide character values in known ranges. */

void test_lc_range (wchar_t wc)
{
  if (wc < 0x40 || 0x49 < wc)
    wc = 0x40;

  int n = __builtin_snprintf (0, 0, "%lc", wc);
  if (n < 0)
    __builtin_abort ();
}

void test_C_range (wchar_t wc)
{
  if (wc < 0x41 || 0x48 < wc)
    wc = 0x41;

  int n = __builtin_snprintf (0, 0, "%C", wc);
  if (n < 0)
    __builtin_abort ();
}

/* Exercise unknown wide character values. */

void test_var (wchar_t wc)
{
  int n = __builtin_snprintf (0, 0, "%lc", wc);
  if (n < 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "abort" 5 "optimized" } } */
