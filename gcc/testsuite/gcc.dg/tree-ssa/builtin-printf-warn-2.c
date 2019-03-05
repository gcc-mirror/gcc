/* PR middle-end/88993 - GCC 9 -Wformat-overflow=2 should reflect real
   libc limits
   Verify that -Wformat-overflow=2 "may exceed" warnings are not issued
   for printf family of functions.
   { dg-do compile }
   { dg-options "-O -Wformat -Wformat-overflow=2 -ftrack-macro-expansion=0" }
   { dg-require-effective-target int32plus } */


#define INT_MAX __INT_MAX__

typedef __SIZE_TYPE__ size_t;

#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

#define T(...) __builtin_printf (__VA_ARGS__)

/* Exercise the "%c" directive with constant arguments.  */

void test_printf_c_const (int width)
{
  /* Verify that a warning is only issued when the output is definitely
     exceeded but not when exceeding it is possible but not inevitable.  */
  T ("%2147483647c", '1');
  T ("X%2147483647c", '2');   /* { dg-warning ".%*c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%2147483647cY", '3');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("%2147483648c", '1');    /* { dg-warning ".%2147483648c. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T ("X%2147483649c", '2');   /* { dg-warning ".%2147483649c. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T ("%2147483650cY", '3');   /* { dg-warning ".%2147483650c. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T ("%*c", INT_MAX, '1');
  T ("X%*c", INT_MAX, '1');   /* { dg-warning ".%*c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%*cY", INT_MAX, '1');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("X%*c", INT_MAX - 1, '1');
  T ("%*cY", INT_MAX - 1, '1');

  T ("%*cY", INT_MAX, '1');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T ("X%*c", INT_MAX, '1');   /* { dg-warning ".%*c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*c", width, '1');
  T ("X%*c", width, '1');
  T ("%*cY", width, '1');

  T ("%*c", width, '1');
  T ("X%*c", width, '1');
  T ("%*cY", width, '1');

  T ("%*c%*c", width, '1', width, '2');
  T ("X%*cY%*cZ", width, '1', width, '2');

  if (width < 4096)
    width = 4096;

  T ("%*c", width, '1');
  T ("X%*c", width, '1');
  T ("%*cY", width, '1');

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*c", width, '1');
  T ("X%*c", width, '2');
  T ("%*cY", width, '3');
  T ("X%*cY", width, '4');    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}


/* Exercise the "%s" directive with constant arguments.  */

void test_printf_s_const (int width, const char *s)
{
  T ("%2147483647s", s);
  T ("X%2147483647s", s);     /* { dg-warning ".%2147483647s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%2147483647sY", s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("%2147483648s", s);      /* { dg-warning "%2147483648s. directive output between 2147483648 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T ("X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output between 2147483649 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T ("%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output between 2147483650 and \[0-9\]+ bytes exceeds .INT_MAX." } */

  T ("%*s", INT_MAX, s);
  T ("X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("X%*s", INT_MAX - 1, s);
  T ("%*sY", INT_MAX - 1, s);

  T ("%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T ("X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  T ("%*s%*s", width, s, width, s);
  T ("X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);
  T ("X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}

/* Exercise the "%ls" directive with constant arguments.  */

void test_printf_ls_const (int width, const wchar_t *s)
{
  T ("%2147483647ls", s);
  T ("X%2147483647ls", s);    /* { dg-warning ".%2147483647ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%2147483647lsY", s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("%2147483648ls", s);     /* { dg-warning "%2147483648ls. directive output between 2147483648 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T ("X%2147483649ls", s);    /* { dg-warning "%2147483649ls. directive output between 2147483649 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T ("%2147483650lsY", s);    /* { dg-warning ".%2147483650ls. directive output between 2147483650 and \[0-9\]+ bytes exceeds .INT_MAX." } */

  T ("%*ls", INT_MAX, s);
  T ("X%*ls", INT_MAX, s);    /* { dg-warning ".%\\\*ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T ("%*lsY", INT_MAX, s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("X%*ls", INT_MAX - 1, s);
  T ("%*lsY", INT_MAX - 1, s);

  T ("%*lsY", INT_MAX, s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T ("X%*ls", INT_MAX, s);     /* { dg-warning ".%\\\*ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*ls", width, s);
  T ("X%*ls", width, s);
  T ("%*lsY", width, s);

  T ("%*ls", width, s);
  T ("X%*ls", width, s);
  T ("%*lsY", width, s);

  T ("%*ls%*ls", width, s, width, s);
  T ("X%*lsY%*lsZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T ("%*ls", width, s);
  T ("X%*ls", width, s);
  T ("%*lsY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*ls", width, s);
  T ("X%*ls", width, s);
  T ("%*lsY", width, s);
  T ("X%*lsY", width, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}


/* Also exercise printf_chk.  */

#undef T
#define T(...) __builtin___printf_chk (__VA_ARGS__)

void test_printf_chk_s_const (int width)
{
  const char *s = "0123456789";

  T (0, "%2147483647s", s);
  T (0, "X%2147483647s", s);    /* { dg-warning ".%2147483647s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (0, "%2147483647sY", s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (0, "%2147483648s", s);      /* { dg-warning "%2147483648s. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T (0, "X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T (0, "%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T (0, "%*s", INT_MAX, s);
  T (0, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (0, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (0, "X%*s", INT_MAX - 1, s);
  T (0, "%*sY", INT_MAX - 1, s);

  T (0, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (0, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (0, "%*s", width, s);
  T (0, "X%*s", width, s);
  T (0, "%*sY", width, s);

  T (0, "%*s", width, s);
  T (0, "X%*s", width, s);
  T (0, "%*sY", width, s);

  T (0, "%*s%*s", width, s, width, s);
  T (0, "X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T (0, "%*s", width, s);
  T (0, "X%*s", width, s);
  T (0, "%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (0, "%*s", width, s);
  T (0, "X%*s", width, s);
  T (0, "%*sY", width, s);
  T (0, "X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}


/* And finally exercise printf_unlocked.  */

#undef T
#define T(...) __builtin_printf_unlocked (__VA_ARGS__)

void test_printf_unlocked_s_const (int width)
{
  const char *s = "0123456789";

  T ("%2147483647s", s);
  T ("X%2147483647s", s);     /* { dg-warning ".%2147483647s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T ("%2147483647sY", s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("%2147483648s", s);      /* { dg-warning "%2147483648s. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T ("X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T ("%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T ("%*s", INT_MAX, s);
  T ("X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T ("%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T ("X%*s", INT_MAX - 1, s);
  T ("%*sY", INT_MAX - 1, s);

  T ("%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T ("X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  T ("%*s%*s", width, s, width, s);
  T ("X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T ("%*s", width, s);
  T ("X%*s", width, s);
  T ("%*sY", width, s);
  T ("X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}
