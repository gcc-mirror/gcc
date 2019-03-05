/* PR middle-end/87041 - -Wformat "reading through null pointer" on
   unreachable code
   Test to verify that the applicable subset of -Wformat-overflow warnings
   are issued for user-defined function declared attribute format printf.
   { dg-do compile }
   { dg-options "-O -Wformat -Wformat-overflow=1 -ftrack-macro-expansion=0" }
   { dg-require-effective-target int32plus } */

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer and objsize macros
   below make use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

#define INT_MAX __INT_MAX__
#define ATTR(...) __attribute__ ((__VA_ARGS__))

typedef __SIZE_TYPE__ size_t;

#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

typedef __WINT_TYPE__ wint_t;

ATTR (format (printf, 2, 3)) void
user_print (char*, const char*, ...);

ATTR (format (printf, 2, 3), nonnull) void
user_print_nonnull (char*, const char*, ...);

ATTR (format (printf, 2, 3), nonnull (2)) void
user_print_nonnull_fmt (char*, const char*, ...);

ATTR (format (printf, 2, 4), nonnull (3)) void
user_print_nonnull_other (char*, const char*, char*, ...);

void dummy_print (char*, const char*, ...);

const char chr_no_nul = 'a';
const char arr_no_nul[] = { 'a', 'b' };


/* Helper to expand function to either __builtin_f or dummy_f to
   make debugging GCC easy.  */
#define T(...)							\
  (((!LINE || LINE == __LINE__)					\
    ? user_print : dummy_print) (0, __VA_ARGS__))

/* Exercise the "%c" directive with constant arguments.  */

void test_user_print_format_string (void)
{
  char *null = 0;
  /* Verify that no warning is issued for a null format string unless
     the corresponding parameter is declared nonnull.  */
  user_print (0, null);
  user_print_nonnull ("x", "y");
  user_print_nonnull ("x", null);   /* { dg-warning "\\\[-Wnonnull]" } */
  user_print_nonnull_fmt (null, "x");
  user_print_nonnull_fmt (0, null); /* { dg-warning "\\\[-Wnonnull]" } */
  user_print_nonnull_other (null, "x", "y");
  user_print_nonnull_other (null, "x", null);  /* { dg-warning "\\\[-Wnonnull]" } */
}


/* Exercise the "%c" directive with constant arguments.  */

void test_user_print_c_const (int width)
{
  /* Verify that a warning is issued for exceeding INT_MAX bytes and
     not otherwise.  */
  T ("%*c",  INT_MAX - 1, '1');
  T ("%*c",  INT_MAX,     '1');
  T ("X%*c", INT_MAX - 1, '1');
  T ("X%*c", INT_MAX,     '1');   /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  T ("%*c%*c", INT_MAX - 1, '1', INT_MAX - 1, '2'); /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  T ("%*cX", INT_MAX - 2, '1');
  T ("%*cX", INT_MAX - 1, '1');
  T ("%*cX", INT_MAX,     '1');   /* { dg-warning "output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;
  T ("%*cX", width, '1');
  T ("%*cXY", width, '1');        /* { dg-warning ".XY. directive output of 2 bytes causes result to exceed .INT_MAX." } */

  /* Also exercise a non-constant format string.  The warning points
     to the line where the format is declared (see bug 87773) so avoid
     triggering that bug here.  */
  const char *fmt = "%*cXYZ";  T (fmt, width, '1');            /* { dg-warning ".XYZ. directive output of 3 bytes causes result to exceed .INT_MAX." } */
}


/* Exercise the "%s" directive with constant arguments.  */

void test_user_print_s_const (int width)
{
  const char *null = 0;

  T ("%s", null);                 /* { dg-warning ".%s. directive argument is null" } */
  T ("%.0s", null);               /* { dg-warning ".%.0s. directive argument is null" } */

  /* Verify no warning is issued for unreachable code.  */
  if (null)
    T ("%s", null);

  T ("%s", &chr_no_nul);          /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */
  T ("%s", arr_no_nul);           /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */

  /* Verify that output in excess of INT_MAX bytes is diagnosed even
     when the size of the destination object is unknown.  */
  T ("%*s",  INT_MAX - 1, "");
  T ("%*s",  INT_MAX,     "");
  T ("X%*s", INT_MAX,     "");    /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;
  T ("%*sX", width, "1");
  T ("%*sXY", width, "1");        /* { dg-warning ".XY. directive output of 2 bytes causes result to exceed .INT_MAX." } */
}


const wchar_t wchr_no_nul = L'a';
const wchar_t warr_no_nul[] = { L'a', L'b' };

/* Exercise the "%s" directive with constant arguments.  */

void test_user_print_ls_const (int width)
{
  const wchar_t *null = 0;

  T ("%ls", null);                /* { dg-warning ".%ls. directive argument is null" } */
  T ("%.0ls", null);              /* { dg-warning ".%.0ls. directive argument is null" } */

  /* Verify no warning is issued for unreachable code.  */
  if (null)
    T ("%ls", null);

  T ("%ls", &wchr_no_nul);        /* { dg-warning ".%ls. directive argument is not a nul-terminated string" } */
  T ("%ls", warr_no_nul);         /* { dg-warning ".%ls. directive argument is not a nul-terminated string" "pr88211" { xfail *-*-* } } */

  /* Verify that output in excess of INT_MAX bytes is diagnosed even
     when the size of the destination object is unknown.  */
  T ("%*ls",  INT_MAX - 1, L"");
  T ("%*ls",  INT_MAX,     L"");
  T ("X%*ls", INT_MAX,     L"");  /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;
  T ("%*lsX", width, L"1");
  T ("%*lsXY", width, L"1");      /* { dg-warning ".XY. directive output of 2 bytes causes result to exceed .INT_MAX." } */
}
