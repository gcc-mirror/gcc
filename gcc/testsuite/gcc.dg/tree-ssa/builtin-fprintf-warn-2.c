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

typedef struct FILE FILE;

FILE *fp;

#define T(...) __builtin_fprintf (__VA_ARGS__)

/* Exercise the "%c" directive with constant arguments.  */

void test_fprintf_c_const (int width)
{
  /* Verify that a warning is only issued when the output is definitely
     exceeded but not when exceeding it is possible but not inevitable.
     Also verify that a note is printed with amount of output produced
     by the call (the result - 1).  */
  T (fp, "%2147483647c", '1');
  T (fp, "X%2147483647c", '2');   /* { dg-warning ".%2147483647c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  /* { dg-message ".__builtin_fprintf. output 2147483649 bytes" "note" { target *-*-* } .-1 } */
  T (fp, "%2147483647cY", '3');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "%2147483648c", '1');    /* { dg-warning ".%2147483648c. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T (fp, "X%2147483649c", '2');   /* { dg-warning ".%2147483649c. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T (fp, "%2147483650cY", '3');   /* { dg-warning ".%2147483650c. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T (fp, "%*c", INT_MAX, '1');
  T (fp, "X%*c", INT_MAX, '1');   /* { dg-warning ".%*c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T (fp, "%*cY", INT_MAX, '1');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%*c", INT_MAX - 1, '1');
  T (fp, "%*cY", INT_MAX - 1, '1');

  T (fp, "%*cY", INT_MAX, '1');   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (fp, "X%*c", INT_MAX, '1');   /* { dg-warning ".%*c. directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
}

/* Exercise the "%c" directive with arguments in a known range.  */

void test_fprintf_c_range (int width)
{
  /* Verify that an known upper bound doesn't trigger a warning.  */
  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*c", width, '1');
  T (fp, "X%*c", width, '1');
  T (fp, "%*cY", width, '1');

  T (fp, "%*c", width, '1');
  T (fp, "X%*c", width, '1');
  T (fp, "%*cY", width, '1');

  T (fp, "%*c%*c", width, '1', width, '2');
  T (fp, "X%*cY%*cZ", width, '1', width, '2');

  /* Verify that a lower bound in excess of 4095 doesn't trigger
     a warning.  */
  if (width < 4096)
    width = 4096;

  T (fp, "%*c", width, '1');
  T (fp, "X%*c", width, '1');
  T (fp, "%*cY", width, '1');

  /* Verify that a large lower bound triggers a warning when the total
     result of the function definitely exceeds INT_MAX.  */
  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*c", width, '1');
  T (fp, "X%*c", width, '2');
  T (fp, "%*cY", width, '3');
  T (fp, "X%*cY", width, '4');    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  /* { dg-message ".__builtin_fprintf. output 2147483649 bytes" "note" { target *-*-* } .-1 } */
}


/* Exercise the "%s" directive.  */

void test_fprintf_s_const (int width, const char *s)
{
  T (fp, "%2147483647s", s);
  T (fp, "%2147483647s", "1");

  T (fp, "%2147483647.2147483647s", s);
  T (fp, "%2147483647.2147483647s", "12");

  T (fp, "X%2147483647s", s);     /* { dg-warning ".%2147483647s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T (fp, "%2147483647sY", s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%2147483647.1s", s);   /* { dg-warning ".%2147483647\\\.1s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (fp, "%2147483647.2sY", s);   /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%1.2147483647s", s);
  T (fp, "%2.2147483647sY", s);

  T (fp, "X%1.2147483647s", "123");
  T (fp, "%2.2147483647sY", "1234");

  T (fp, "%2147483648s", s);      /* { dg-warning "%2147483648s. directive output between 2147483648 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T (fp, "X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output between 2147483649 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T (fp, "%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output between 2147483650 and \[0-9\]+ bytes exceeds .INT_MAX." } */

  T (fp, "%*s", INT_MAX, s);
  T (fp, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T (fp, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%*s", INT_MAX - 1, s);
  T (fp, "%*sY", INT_MAX - 1, s);

  T (fp, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (fp, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  T (fp, "%*s%*s", width, s, width, s);
  T (fp, "X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);
  T (fp, "X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}

/* Exercise the "%ls" directive.  */

void test_fprintf_ls_const (int width, const wchar_t *s)
{
  T (fp, "%2147483647ls", s);
  T (fp, "X%2147483647ls", s);    /* { dg-warning ".%2147483647ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T (fp, "%2147483647lsY", s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "%2147483648ls", s);     /* { dg-warning "%2147483648ls. directive output between 2147483648 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T (fp, "X%2147483649ls", s);    /* { dg-warning "%2147483649ls. directive output between 2147483649 and \[0-9\]+ bytes exceeds .INT_MAX." } */
  T (fp, "%2147483650lsY", s);    /* { dg-warning ".%2147483650ls. directive output between 2147483650 and \[0-9\]+ bytes exceeds .INT_MAX." } */

  T (fp, "%*ls", INT_MAX, s);
  T (fp, "X%*ls", INT_MAX, s);    /* { dg-warning ".%\\\*ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
  T (fp, "%*lsY", INT_MAX, s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%*ls", INT_MAX - 1, s);
  T (fp, "%*lsY", INT_MAX - 1, s);

  T (fp, "%*lsY", INT_MAX, s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (fp, "X%*ls", INT_MAX, s);     /* { dg-warning ".%\\\*ls. directive output between 2147483647 and \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*ls", width, s);
  T (fp, "X%*ls", width, s);
  T (fp, "%*lsY", width, s);

  T (fp, "%*ls", width, s);
  T (fp, "X%*ls", width, s);
  T (fp, "%*lsY", width, s);

  T (fp, "%*ls%*ls", width, s, width, s);
  T (fp, "X%*lsY%*lsZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T (fp, "%*ls", width, s);
  T (fp, "X%*ls", width, s);
  T (fp, "%*lsY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*ls", width, s);
  T (fp, "X%*ls", width, s);
  T (fp, "%*lsY", width, s);
  T (fp, "X%*lsY", width, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}


/* Also exercise fprintf_chk.  */

#undef T
#define T(...) __builtin___fprintf_chk (__VA_ARGS__)

void test_fprintf_chk_s_const (int width)
{
  const char *s = "0123456789";

  T (fp, 0, "%2147483647s", s);
  T (fp, 0, "X%2147483647s", s);    /* { dg-warning ".%2147483647s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (fp, 0, "%2147483647sY", s);    /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, 0, "%2147483648s", s);      /* { dg-warning "%2147483648s. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T (fp, 0, "X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T (fp, 0, "%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T (fp, 0, "%*s", INT_MAX, s);
  T (fp, 0, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (fp, 0, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, 0, "X%*s", INT_MAX - 1, s);
  T (fp, 0, "%*sY", INT_MAX - 1, s);

  T (fp, 0, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (fp, 0, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, 0, "%*s", width, s);
  T (fp, 0, "X%*s", width, s);
  T (fp, 0, "%*sY", width, s);

  T (fp, 0, "%*s", width, s);
  T (fp, 0, "X%*s", width, s);
  T (fp, 0, "%*sY", width, s);

  T (fp, 0, "%*s%*s", width, s, width, s);
  T (fp, 0, "X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T (fp, 0, "%*s", width, s);
  T (fp, 0, "X%*s", width, s);
  T (fp, 0, "%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, 0, "%*s", width, s);
  T (fp, 0, "X%*s", width, s);
  T (fp, 0, "%*sY", width, s);
  T (fp, 0, "X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}


/* And finally exercise fprintf_unlocked.  */

#undef T
#define T(...) __builtin_fprintf_unlocked (__VA_ARGS__)

void test_fprintf_unlocked_s_const (int width)
{
  const char *s = "0123456789";

  T (fp, "%2147483647s", s);
  T (fp, "X%2147483647s", s);     /* { dg-warning ".%2147483647s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (fp, "%2147483647sY", s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "%2147483648s", s);      /* { dg-warning "%2147483648s. directive output of 2147483648 bytes exceeds .INT_MAX." } */
  T (fp, "X%2147483649s", s);     /* { dg-warning "%2147483649s. directive output of 2147483649 bytes exceeds .INT_MAX." } */
  T (fp, "%2147483650sY", s);     /* { dg-warning ".%2147483650s. directive output of 2147483650 bytes exceeds .INT_MAX." } */

  T (fp, "%*s", INT_MAX, s);
  T (fp, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */
  T (fp, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */

  T (fp, "X%*s", INT_MAX - 1, s);
  T (fp, "%*sY", INT_MAX - 1, s);

  T (fp, "%*sY", INT_MAX, s);     /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
  T (fp, "X%*s", INT_MAX, s);     /* { dg-warning ".%\\\*s. directive output of 2147483647 bytes causes result to exceed .INT_MAX." } */

  if (width > INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  T (fp, "%*s%*s", width, s, width, s);
  T (fp, "X%*sY%*sZ", width, s, width, s);

  if (width < 4096)
    width = 4096;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);

  if (width < INT_MAX - 1)
    width = INT_MAX - 1;

  T (fp, "%*s", width, s);
  T (fp, "X%*s", width, s);
  T (fp, "%*sY", width, s);
  T (fp, "X%*sY", width, s);      /* { dg-warning ".Y. directive output of 1 bytes causes result to exceed .INT_MAX." } */
}
