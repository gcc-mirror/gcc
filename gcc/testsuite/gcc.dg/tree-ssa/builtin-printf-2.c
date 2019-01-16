/* Verify that tests for the result of calls to fprintf, printf, vfprintf,
   and vprintf are not eliminated, even if it is possible to determine
   their value on success (the calls may fail and return a negative value).
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

typedef struct FILE FILE;
typedef __builtin_va_list va_list;

extern int printf (const char *, ...);
extern int printf_unlocked (const char *, ...);
extern int vprintf (const char *, va_list);

extern int fprintf (FILE*, const char *, ...);
extern int fprintf_unlocked (FILE*, const char *, ...);
extern int vfprintf (FILE*, const char *, va_list);

#define fprintf_chk    __builtin___fprintf_chk
#define printf_chk     __builtin___printf_chk
#define vfprintf_chk   __builtin___vfprintf_chk
#define vprintf_chk    __builtin___vprintf_chk

#define CAT(s, n)   s ## n

#define KEEP(func, line)   CAT (func ## _test_on_line_, line)

/* Emit one call to a function named call_on_line_NNN when the result
   of the call FUNC ARGS is less than zero, zero, or greater than zero.
   This verifies that the expression is not eliminated.

   For known output it is possible to bound the return value to
   [INT_MIN, -1] U [0, N] with N being the size of the output, but
   that optimization isn't implemented (yet).  */

#define T(func, args)						\
  do {								\
    extern void KEEP (func, __LINE__)(const char*);		\
    if ((func args) < 0) KEEP (func, __LINE__)("< 0");		\
    if ((func args) >= 0) KEEP (func, __LINE__)(">= 0");	\
  } while (0)

void test_fprintf (FILE *f, const char *s)
{
  /* Here the result is in [INT_MIN, 0], i.e., it cannot be positive.
     It might be a useful enhancement to implement this optimization.  */
  T (fprintf, (f, ""));
  T (fprintf, (f, "1"));
  T (fprintf, (f, "123"));
  T (fprintf, (f, s));

  T (fprintf, (f, "%c", 0));
  T (fprintf, (f, "%c", '1'));
  T (fprintf, (f, "%c", *s));

  T (fprintf, (f, "%s", ""));
  T (fprintf, (f, "%s", "1"));
  T (fprintf, (f, "%.0s", ""));
  T (fprintf, (f, "%.0s", s));

  /* { dg-final { scan-tree-dump-times " fprintf_test_on_line_" 22 "optimized"} } */
}


void test_fprintf_unlocked (FILE *f, const char *s)
{
  T (fprintf_unlocked, (f, ""));
  T (fprintf_unlocked, (f, "1"));
  T (fprintf_unlocked, (f, "123"));
  T (fprintf_unlocked, (f, s));

  T (fprintf_unlocked, (f, "%c", 0));
  T (fprintf_unlocked, (f, "%c", '1'));
  T (fprintf_unlocked, (f, "%c", *s));

  T (fprintf_unlocked, (f, "%s", ""));
  T (fprintf_unlocked, (f, "%s", "1"));
  T (fprintf_unlocked, (f, "%.0s", ""));
  T (fprintf_unlocked, (f, "%.0s", s));

  /* { dg-final { scan-tree-dump-times " fprintf_unlocked_test_on_line_" 22 "optimized"} } */
}


void test_fprintf_chk (FILE *f, const char *s)
{
  T (fprintf_chk, (f, 0, ""));
  T (fprintf_chk, (f, 0, "1"));
  T (fprintf_chk, (f, 0, "123"));
  T (fprintf_chk, (f, 0, s));

  T (fprintf_chk, (f, 0, "%c", 0));
  T (fprintf_chk, (f, 0, "%c", '1'));
  T (fprintf_chk, (f, 0, "%c", *s));

  T (fprintf_chk, (f, 0, "%s", ""));
  T (fprintf_chk, (f, 0, "%s", "1"));
  T (fprintf_chk, (f, 0, "%.0s", ""));
  T (fprintf_chk, (f, 0, "%.0s", s));

  /* { dg-final { scan-tree-dump-times " __builtin___fprintf_chk_test_on_line_" 22 "optimized"} } */
}


void test_vfprintf (FILE *f, va_list va)
{
  T (vfprintf, (f, "", va));
  T (vfprintf, (f, "123", va));

  T (vfprintf, (f, "%c", va));

  T (vfprintf, (f, "%.0s", va));

  /* { dg-final { scan-tree-dump-times " vfprintf_test_on_line_" 8 "optimized"} } */
}


void test_vfprintf_chk (FILE *f, va_list va)
{
  T (vfprintf_chk, (f, 0, "", va));
  T (vfprintf_chk, (f, 0, "123", va));

  T (vfprintf_chk, (f, 0, "%c", va));

  T (vfprintf_chk, (f, 0, "%.0s", va));

  /* { dg-final { scan-tree-dump-times " __builtin___vfprintf_chk_test_on_line_" 8 "optimized"} } */
}


void test_printf (const char *s)
{
  T (printf, (""));
  T (printf, ("1"));
  T (printf, ("123"));
  T (printf, (s));

  T (printf, ("%c", 0));
  T (printf, ("%c", '1'));
  T (printf, ("%c", *s));

  T (printf, ("%s", ""));
  T (printf, ("%s", "1"));
  T (printf, ("%.0s", ""));
  T (printf, ("%.0s", s));

/* { dg-final { scan-tree-dump-times " printf_test_on_line_" 22 "optimized"} } */
}


void test_printf_unlocked (const char *s)
{
  T (printf_unlocked, (""));
  T (printf_unlocked, ("1"));
  T (printf_unlocked, ("123"));
  T (printf_unlocked, (s));

  T (printf_unlocked, ("%c", 0));
  T (printf_unlocked, ("%c", '1'));
  T (printf_unlocked, ("%c", *s));

  T (printf_unlocked, ("%s", ""));
  T (printf_unlocked, ("%s", "1"));
  T (printf_unlocked, ("%.0s", ""));
  T (printf_unlocked, ("%.0s", s));

/* { dg-final { scan-tree-dump-times " printf_unlocked_test_on_line_" 22 "optimized"} } */
}


void test_printf_chk (const char *s)
{
  T (printf_chk, (0, ""));
  T (printf_chk, (0, "1"));
  T (printf_chk, (0, "123"));
  T (printf_chk, (0, s));

  T (printf_chk, (0, "%c", 0));
  T (printf_chk, (0, "%c", '1'));
  T (printf_chk, (0, "%c", *s));

  T (printf_chk, (0, "%s", ""));
  T (printf_chk, (0, "%s", "1"));
  T (printf_chk, (0, "%.0s", ""));
  T (printf_chk, (0, "%.0s", s));

/* { dg-final { scan-tree-dump-times " __builtin___printf_chk_test_on_line_" 22 "optimized"} } */
}


void test_vprintf (va_list va)
{
  T (vprintf, ("", va));
  T (vprintf, ("123", va));

  T (vprintf, ("%c", va));

  T (vprintf, ("%.0s", va));

  /* { dg-final { scan-tree-dump-times " vprintf_test_on_line_" 8 "optimized"} } */
}


void test_vprintf_chk (va_list va)
{
  T (vprintf_chk, (0, "", va));
  T (vprintf_chk, (0, "123", va));

  T (vprintf_chk, (0, "%c", va));

  T (vprintf_chk, (0, "%.0s", va));

  /* { dg-final { scan-tree-dump-times " __builtin___vprintf_chk_test_on_line_" 8 "optimized"} } */
}
