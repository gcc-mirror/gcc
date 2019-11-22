/* PR middle-end/87041 - -Wformat "reading through null pointer" on
   unreachable code
   Test to verify that the applicable subset of -Wformat-overflow warnings
   are issued for the fprintf function.
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

typedef __SIZE_TYPE__ size_t;

#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

typedef __WINT_TYPE__ wint_t;

void sink (void*, ...);

/* Declare as void* to work around bug 87775.  */
typedef void FILE;

int dummy_fprintf (FILE*, const char*, ...);

FILE *fp;

const char chr_no_nul = 'a';
const char arr_no_nul[] = { 'a', 'b' };


/* Helper to expand function to either __builtin_f or dummy_f to
   make debugging GCC easy.  */
#define T(...)							\
  (((!LINE || LINE == __LINE__)					\
    ? __builtin_fprintf : dummy_fprintf) (fp, __VA_ARGS__))

/* Exercise the "%c" directive with constant arguments.  */

void test_fprintf_c_const (int width)
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

void test_fprintf_s_const (int width)
{
  const char *nulptr = 0;

  T ("%s", nulptr);               /* { dg-warning "\\\[-Wformat|-Wnonnull" } */
  T ("%.0s", nulptr);             /* { dg-warning ".%.0s. directive argument is null" } */

  /* Verify no warning is issued for unreachable code.  */
  if (nulptr)
    T ("%s", nulptr);

  T ("%s", &chr_no_nul);          /* { dg-warning ".%s. directive argument is not a nul-terminated string|argument missing terminating nul" } */
  T ("%s", arr_no_nul);           /* { dg-warning ".%s. directive argument is not a nul-terminated string|argument missing terminating nul" } */

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

void test_fprintf_ls_const (int width)
{
  const wchar_t *nulptr = 0;

  T ("%ls", nulptr);              /* { dg-warning ".%ls. directive argument is null" } */
  T ("%.0ls", nulptr);            /* { dg-warning ".%.0ls. directive argument is null" } */

  /* Verify no warning is issued for unreachable code.  */
  if (nulptr)
    T ("%ls", nulptr);

  T ("%ls", &wchr_no_nul);        /* { dg-warning ".%ls. directive argument is not a nul-terminated string" } */
  T ("%ls", warr_no_nul);         /* { dg-warning ".%ls. directive argument is not a nul-terminated string" "pr88226" { xfail *-*-* } } */

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
