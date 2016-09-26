/* { dg-do compile } */
/* { dg-options "-std=c99 -Wformat -Wformat-length=1 -ftrack-macro-expansion=0" } */

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer and objsize macros
   below make use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

#define INT_MAX __INT_MAX__

char buffer [256];
extern char *ptr;

/* Evaluate to an array of SIZE characters when non-negative and LINE
   is not set or set to the line the macro is on, or to a pointer to
   an unknown object otherwise.  */
#define buffer(size)							\
  (0 <= size && (!LINE || __LINE__ == LINE)				\
   ? buffer + sizeof buffer - size : ptr)

/* Evaluate to SIZE when non-negative and LINE is not set or set to
   the line the macro is on, or to SIZE_MAX otherise.  */
#define objsize(size)							\
  (0 <= size && (!LINE || __LINE__ == LINE)				\
   ? size : __SIZE_MAX__)

typedef __SIZE_TYPE__ size_t;

#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

typedef __WINT_TYPE__ wint_t;

typedef unsigned char UChar;

const char s0[] = "";
const char s1[] = "1";
const char s2[] = "12";
const char s3[] = "123";
const char s4[] = "1234";
const char s5[] = "12345";
const char s6[] = "123456";
const char s7[] = "1234567";
const char s8[] = "12345678";

void sink (void*, ...);

/* Macro to verify that calls to __builtin_sprintf (i.e., with no size
   argument) issue diagnostics by correctly determining the size of
   the destination buffer.  */
#define T(size, fmt, ...)						\
  __builtin_sprintf (buffer (size), fmt, __VA_ARGS__),			\
    sink (buffer, ptr);

/* Exercise the "%c" and "%lc" directive with constant arguments.  */

void test_sprintf_c_const (void)
{
  T (-1, "%c",    0);           /* No warning for unknown destination size.  */
  T ( 0, "%c",    0);           /* { dg-warning ".%c. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%c",    0);           /* { dg-warning "writing a terminating nul past the end" } */
  T ( 1, "%c",   '1');          /* { dg-warning "nul past the end" } */
  T ( 2, "%c",   '1');
  T ( 2, "%2c",  '1');          /* { dg-warning "nul past the end" } */
  T ( 2, "%3c",  '1');          /* { dg-warning "into a region" } */
  T ( 2, "%c%c", '1', '2');     /* { dg-warning "nul past the end" } */
  T ( 3, "%c%c", '1', '2');

  T ( 2, "%1$c%2$c", '1', '2'); /* { dg-warning "does not support %n.|nul past the end" } */
  T ( 3, "%1$c%2$c", '1', '2');

  /* Verify that a warning is issued for exceeding INT_MAX bytes and
     not otherwise.  */
  T (-1, "%*c",  INT_MAX - 1, '1');
  T (-1, "%*c",  INT_MAX,     '1');
  T (-1, "X%*c", INT_MAX - 1, '1');
  T (-1, "X%*c", INT_MAX,     '1'); /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  T (-1, "%*c%*c", INT_MAX - 1, '1', INT_MAX - 1, '2'); /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  T (-1, "%*cX", INT_MAX - 2, '1');
  T (-1, "%*cX", INT_MAX - 1, '1');
  T (-1, "%*cX", INT_MAX,     '1'); /* { dg-warning "output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */
}

/* Exercise the "%p" directive with constant arguments.  */

void test_sprintf_p_const (void)
{
  /* GLIBC and uClibc format null pointers as "(nil)".  Sane implementations
     format null pointers as 0 or 0x0 and so the following will only be
     diagnosed on the former targets.  */
  T (5, "%p",     (void*)0);
  /* { dg-warning "nul past the end" "(nil)" { target *-linux-gnu *-*-uclinux } 96 } */

  /* The exact output for %p is unspecified by C.  Two formats are known:
     same as %tx (for example AIX) and same as %#tx (for example Solaris).  */
  T (0, "%p",     (void*)0x1);    /* { dg-warning ".%p. directive writing . bytes? into a region of size 0" } */
  T (1, "%p",     (void*)0x12);   /* { dg-warning ".%p. directive writing . bytes? into a region of size 1" } */
  T (2, "%p",     (void*)0x123);  /* { dg-warning ".%p. directive writing . bytes? into a region of size 2" } */

  /* GLIBC and uClibc treat the ' ' flag with the "%p" directive the same
     as with signed integer conversions (i.e., it prepends a space).  Other
     known implementations ignore it.  */
  T (6, "% p",    (void*)0x234);  /* { dg-warning ". . flag used with .%p." } */
  /* { dg-warning "nul past the end" "Glibc %p" { target *-linux-gnu } 108 } */
  /* { dg-warning "nul past the end" "Generic %p" { target *-*-uclinux } 108 } */
}

/* Verify that no warning is issued for calls that write into a flexible
   array member whose size isn't known.  Also verify that calls that use
   a flexible array member as an argument to the "%s" directive do not
   cause a warning.  */

void test_sprintf_flexarray (void *p, int i)
{
  struct S
  {
    int n;
    char a [];
  } *s = p;

  __builtin_sprintf (s->a, "%c",       'x');

  __builtin_sprintf (s->a, "%s",       "");
  __builtin_sprintf (s->a, "%s",       "abc");
  __builtin_sprintf (s->a, "abc%sghi", "def");

  __builtin_sprintf (s->a, "%i",       1234);

  __builtin_sprintf (buffer (1), "%s",  s->a);
  __builtin_sprintf (buffer (1), "%s",  s [i].a);
}

/* Same as above but for zero-length arrays.  */

void test_sprintf_zero_length_array (void *p, int i)
{
  struct S
  {
    int n;
    char a [0];
  } *s = p;

  __builtin_sprintf (s->a, "%c",       'x');

  __builtin_sprintf (s->a, "%s",       "");
  __builtin_sprintf (s->a, "%s",       "abc");
  __builtin_sprintf (s->a, "abc%sghi", "def");

  __builtin_sprintf (s->a, "%i",       1234);

  __builtin_sprintf (buffer (1), "%s",  s->a);
  __builtin_sprintf (buffer (1), "%s",  s [i].a);
}

/* Verify that the note printed along with the diagnostic mentions
   the correct sizes and refers to the location corresponding to
   the affected directive.  */

void test_sprintf_note (void)
{
#define P __builtin_sprintf

  /* Diagnostic column numbers are 1-based.  */

  P (buffer (0),                /* { dg-message "format output 4 bytes into a destination of size 0" } */
     "%c%s%i", '1', "2", 3);    /* { dg-warning "7:.%c. directive writing 1 byte into a region of size 0" } */

  P (buffer (1),                /* { dg-message "format output 6 bytes into a destination of size 1" } */
     "%c%s%i", '1', "23", 45);  /* { dg-warning "9:.%s. directive writing 2 bytes into a region of size 0" } */

  P (buffer (2),                /* { dg-message "format output 6 bytes into a destination of size 2" } */
     "%c%s%i", '1', "2", 345);  /* { dg-warning "11:.%i. directive writing 3 bytes into a region of size 0" } */

  /* It would be nice if the caret in the location range for the format
     string below could be made to point at the closing quote of the format
     string, like so:
       sprintf (d, "%c%s%i", '1', "2", 3456);
	            ~~~~~~^
     Unfortunately, that doesn't work with the current setup.  */
  P (buffer (6),                /* { dg-message "format output 7 bytes into a destination of size 6" } */
     "%c%s%i", '1', "2", 3456); /* { dg-warning "writing a terminating nul past the end of the destination" } */
}

#undef T
#define T(size, fmt, ...)					  \
  __builtin___sprintf_chk (buffer (size), 0, objsize (size), fmt, \
			   __VA_ARGS__), sink (buffer, ptr)

/* Exercise the "%c" and "%lc" directive with constant arguments.  */

void test_sprintf_chk_c_const (void)
{
  T (-1, "%c",    0);            /* No warning for unknown destination size.  */
  /* Verify the full text of the diagnostic for just the distinct messages
     and use abbreviations in subsequent test cases.  */
  T (0, "%c",     0);            /* { dg-warning ".%c. directive writing 1 byte into a region of size 0" } */
  T (1, "%c",     0);            /* { dg-warning "writing a terminating nul past the end" } */
  T (1, "%c",   '1');            /* { dg-warning "nul past the end" } */
  T (2, "%c",   '1');
  T (2, "%2c",  '1');            /* { dg-warning "nul past the end" } */
  T (2, "%3c",  '1');            /* { dg-warning "into a region" } */
  T (2, "%c%c", '1', '2');       /* { dg-warning "nul past the end" } */
  T (3, "%c%c", '1', '2');

  /* Wide characters.  */
  T (0, "%lc",     0);           /* { dg-warning "nul past the end" } */
  T (1, "%lc",     0);
  T (1, "%lc%lc",  0, 0);
  T (2, "%lc",     0);
  T (2, "%lc%lc",  0, 0);

  /* The following could result in as few as no bytes and in as many as
     MB_CUR_MAX, but since the MB_CUR_MAX value is a runtime property
     the write cannot be reliably diagnosed.  */
  T (2, "%lc",  (wint_t)L'1');
  T (2, "%1lc", (wint_t)L'1');
  /* Writing some unknown number of bytes into a field two characters wide.  */
  T (2, "%2lc", (wint_t)L'1');          /* { dg-warning "nul past the end" } */

  T (3, "%lc%c",   (wint_t)L'1', '2');
  /* Here in the best case each argument will format as single character,
     causing the terminating NUL to be written past the end.  */
  T (3, "%lc%c%c", (wint_t)L'1', '2', '3');   /* { dg-warning "nul past the end" } */
  T (3, "%lc%lc%c", (wint_t)L'1', (wint_t)L'2', '3'); /* { dg-warning "nul past the end" } */
}

/* Exercise the "%s" and "%ls" directive with constant arguments.  */

void test_sprintf_chk_s_const (void)
{
  T (-1, "%*s",  0, "");        /* No warning for unknown destination size.  */
  T ( 0, "%*s",  0, "");        /* { dg-warning "nul past the end" } */
  T ( 0, "%-s",     "");        /* { dg-warning "nul past the end" } */
  T ( 0, "%*s",  0, s0);        /* { dg-warning "nul past the end" } */
  T ( 1, "%*s",  0, "");
  T ( 1, "%*s",  0, s0);
  T ( 1, "%*s",  0, "\0");
  T ( 1, "%*s",  0, "1");       /* { dg-warning "nul past the end" } */
  T ( 1, "%*s",  0, s1);        /* { dg-warning "nul past the end" } */
  T ( 1, "%1s",     "");        /* { dg-warning "nul past the end" } */
  T ( 1, "%1s",     s0);        /* { dg-warning "nul past the end" } */
  T (-1, "%1s",    "1");        /* No warning for unknown destination size.  */
  T ( 1, "%*s",  1, "");        /* { dg-warning "nul past the end" } */
  T ( 1, "%*s",  1, s0);        /* { dg-warning "nul past the end" } */
  T (-1, "%*s",  1, s0);        /* No warning for unknown destination size.  */

  T (1, "%.0s",    "123");
  T (1, "%.0s",    s3);
  T (1, "%.*s", 0, "123");
  T (1, "%.*s", 0, s3);
  T (1, "%.1s",    "123");      /* { dg-warning "nul past the end" } */
  T (1, "%.1s",    s3);         /* { dg-warning "nul past the end" } */
  T (1, "%.*s", 1, "123");      /* { dg-warning "nul past the end" } */
  T (1, "%.*s", 1, s3);         /* { dg-warning "nul past the end" } */

  T (2, "%.*s", 0, "");
  T (2, "%.*s", 0, "1");
  T (2, "%.*s", 0, s1);
  T (2, "%.*s", 0, "1\0");
  T (2, "%.*s", 0, "12");
  T (2, "%.*s", 0, s2);

  T (2, "%.*s", 1, "");
  T (2, "%.*s", 1, "1");
  T (2, "%.*s", 1, s1);
  T (2, "%.*s", 1, "1\0");
  T (2, "%.*s", 1, "12");
  T (2, "%.*s", 1, s2);

  T (2, "%.*s", 2, "");
  T (2, "%.*s", 2, "1");
  T (2, "%.*s", 2, s1);
  T (2, "%.*s", 2, "1\0");
  T (2, "%.*s", 2, "12");       /* { dg-warning "nul past the end" } */
  T (2, "%.*s", 2, s2);         /* { dg-warning "nul past the end" } */

  T (2, "%.*s", 3, "");
  T (2, "%.*s", 3, "1");
  T (2, "%.*s", 3, s1);
  T (2, "%.*s", 3, "1\0");
  T (2, "%.*s", 3, "12");       /* { dg-warning "nul past the end" } */
  T (2, "%.*s", 3, "123");      /* { dg-warning "into a region" } */
  T (2, "%.*s", 3, s2);         /* { dg-warning "nul past the end" } */
  T (2, "%.*s", 3, s3);         /* { dg-warning "into a region" } */

  T (2, "%*s",  0, "");
  T (2, "%*s",  0, "1");
  T (2, "%*s",  0, s1);
  T (2, "%*s",  0, "1\0");
  T (2, "%*s",  0, "12");       /* { dg-warning "nul past the end" } */
  T (2, "%*s",  0, s2);         /* { dg-warning "nul past the end" } */

  /* Verify that output in excess of INT_MAX bytes is diagnosed even
     when the size of the destination object is unknown.  */
  T (-1, "%*s",  INT_MAX - 1, "");
  T (-1, "%*s",  INT_MAX,     "");
  T (-1, "X%*s", INT_MAX,     ""); /* { dg-warning "directive output of \[0-9\]+ bytes causes result to exceed .INT_MAX." } */

  /* Multiple directives.  */

  T (1, "%s%s", "", "");
  T (1, "%s%s", s0, s0);
  T (1, "%s%s", "", "1");       /* { dg-warning "nul past the end" } */
  T (1, "%s%s", s0, s1);        /* { dg-warning "nul past the end" } */
  T (1, "%s%s", "1", "");       /* { dg-warning "nul past the end" } */
  T (1, "%s%s", s1, s0);        /* { dg-warning "nul past the end" } */
  T (1, "%s%s", "1", "2");      /* { dg-warning "into a region" } */
  T (1, "%s%s", s1, s1);        /* { dg-warning "into a region" } */

  T (2, "%s%s", "", "");
  T (2, "%s%s", "", "1");
  T (2, "%s%s", "1", "");
  T (2, "%s%s", "", "12");      /* { dg-warning "nul past the end" } */
  T (2, "%s%s", "1", "2");      /* { dg-warning "nul past the end" } */
  T (2, "%s%s", "12", "2");     /* { dg-warning "into a region" } */
  T (2, "%s%s", "1", "23");     /* { dg-warning "into a region" } */
  T (2, "%s%s", "12", "3");     /* { dg-warning "into a region" } */
  T (2, "%s%s", "12", "34");    /* { dg-warning "into a region" } */

  T (2, "_%s",   "");
  T (2, "%%%s",  "");
  T (2, "%s%%",  "");
  T (2, "_%s",   "1");          /* { dg-warning "nul past the end" } */
  T (2, "%%%s",  "1");          /* { dg-warning "nul past the end" } */
  T (2, "%s%%",  "1");          /* { dg-warning "nul past the end" } */
  T (2, "_%s",   "12");         /* { dg-warning "into a region" } */
  T (2, "__%s",  "1");          /* { dg-warning "into a region" } */

  T (2, "%1$s%2$s", "12", "3"); /* { dg-warning ".%2.s. directive writing 1 byte into a region of size 0" } */
  T (2, "%1$s%1$s", "12");      /* { dg-warning "does not support|.%1.s. directive writing 2 bytes into a region of size 0" } */
  T (2, "%2$s%1$s", "1", "23"); /* { dg-warning ".%1.s. directive writing 1 byte into a region of size 0" } */
  T (2, "%2$s%2$s", "1", "23"); /* { dg-warning "unused|%2.s. directive writing 2 bytes into a region of size 0" } */

  T (3, "__%s", "");
  T (3, "__%s", "1");           /* { dg-warning "nul past the end" } */
  T (3, "%s_%s", "", "");
  T (3, "%s_%s", "1", "");
  T (3, "%s_%s", "", "1");
  T (3, "%s_%s", "1", "2");     /* { dg-warning "nul past the end" } */

  /* Wide strings.  */
  T (-1, "%ls",      L"");
  T ( 0, "%ls",      L"");      /* { dg-warning "nul past the end" } */
  T ( 1, "%ls",      L"");
  T ( 1, "%ls",      L"\0");
  T ( 1, "%1ls",     L"");      /* { dg-warning "nul past the end" } */

  T (0, "%*ls",  0, L"");       /* { dg-warning "nul past the end" } */
  T (1, "%*ls",  0, L"");
  T (1, "%*ls",  0, L"\0");
  T (1, "%*ls",  1, L"");       /* { dg-warning "nul past the end" } */

  T (1, "%ls",      L"1");      /* { dg-warning "nul past the end" } */
  T (1, "%.0ls",    L"1");
  T (2, "%.0ls",    L"1");
  T (2, "%.1ls",    L"1");
  T (2, "%.*ls", 1, L"1");

  /* The "%.2ls" directive below will write at a minimum 1 byte (because
     L"1" is known and can be assumed to convert to at least one multibyte
     character), and at most 2 bytes because of the precision.  Since its
     output is explicitly bounded it is diagnosed.  */
  T (2, "%.2ls",    L"1");      /* { dg-warning "nul past the end" } */
  T (2, "%.*ls", 2, L"1");      /* { dg-warning "nul past the end" } */

  T (3, "%.0ls",    L"1");
  T (3, "%.1ls",    L"1");
  T (3, "%.2ls",    L"1");
}

/* Exercise the "%hhd", "%hhi", "%hho", "%hhu", and "%hhx" directives
   with constant arguments.  */

void test_sprintf_chk_hh_const (void)
{
  T (-1, "%hhd",        0);

  T (1, "%hhd",         0);     /* { dg-warning "nul past the end" } */
  T (1, "%hhd",         1);     /* { dg-warning "nul past the end" } */
  T (1, "%hhd",        -1);     /* { dg-warning "into a region" } */
  T (1, "%+hhd",        0);     /* { dg-warning "into a region" } */
  T (1, "%+hhd",        1);     /* { dg-warning "into a region" } */
  T (1, "%-hhd",        0);     /* { dg-warning "nul past the end" } */

  T (1, "%hhi",         0);     /* { dg-warning "nul past the end" } */
  T (1, "%hhi",         1);     /* { dg-warning "nul past the end" } */
  T (1, "%hhi",        -1);     /* { dg-warning "into a region" } */
  T (1, "%+hhi",        0);     /* { dg-warning "into a region" } */
  T (1, "%+hhi",        1);     /* { dg-warning "into a region" } */
  T (1, "%-hhi",        0);     /* { dg-warning "nul past the end" } */

  T (2, "%hhi",         0);
  T (2, "%hhi",         1);
  T (2, "%hhi",         9);
  T (2, "% hhi",        9);     /* { dg-warning "nul past the end" } */
  T (2, "%+hhi",        9);     /* { dg-warning "nul past the end" } */
  T (2, "%-hhi",        9);
  T (2, "%hhi",        10);     /* { dg-warning "nul past the end" } */
  T (2, "%hhi",        -1);     /* { dg-warning "nul past the end" } */
  T (2, "% hhi",       -1);     /* { dg-warning "nul past the end" } */
  T (2, "%+hhi",       -1);     /* { dg-warning "nul past the end" } */
  T (2, "%-hhi",       -1);     /* { dg-warning "nul past the end" } */

  T (2, "%hho",         0);
  T (2, "%hho",         1);
  T (2, "%hho",         7);
  T (2, "%hho",       010);     /* { dg-warning "nul past the end" } */
  T (2, "%hho",       077);     /* { dg-warning "nul past the end" } */
  T (2, "%hho",        -1);     /* { dg-warning "into a region" } */

  T (2, "%hhx",         0);
  T (2, "%hhX",         1);
  T (2, "%hhx",         7);
  T (2, "%hhX",         8);
  T (2, "%hhx",        -1);     /* { dg-warning "nul past the end" } */
  T (2, "%hhX",       0xf);
  T (2, "%hhx",      0x10);     /* { dg-warning "nul past the end" } */
  T (2, "%hhX",      0xff);     /* { dg-warning "nul past the end" } */

  T (1, "%#hhx",        0);     /* { dg-warning "nul past the end" } */
  T (2, "%#hhx",        0);
  T (3, "%#hhx",        1);     /* { dg-warning "nul past the end" } */

  T (4, "%hhd",       255);
  T (4, "%hhd",       256);
  T (4, "%hhd",     0xfff);
  T (4, "%hhd",    0xffff);

  T (4, "%hhi",       255);
  T (4, "%hhi",       256);
  T (4, "%hhi",     0xfff);
  T (4, "%hhi",    0xffff);

  T (4, "%hhu",        -1);
  T (4, "%hhu",       255);
  T (4, "%hhu",       256);
  T (4, "%hhu",     0xfff);
  T (4, "%hhu",    0xffff);

  T (4, "%#hhx",        0);
  T (4, "%#hhx",        1);
  T (4, "%#hhx",       -1);     /* { dg-warning "nul past the end" } */
  T (4, "%#hhx",      0xf);
  T (4, "%#hhx",     0x10);     /* { dg-warning "nul past the end" } */
  T (4, "%#hhx",     0xff);     /* { dg-warning "nul past the end" } */
  T (4, "%#hhx",    0xfff);     /* { dg-warning "nul past the end" } */

  T (4, "%hhi %hhi",  0,  0);
  T (4, "%hhi %hhi",  9,  9);
  T (4, "%hhi %hhi",  1, 10);   /* { dg-warning "nul past the end" } */
  T (4, "%hhi %hhi", 10,  1);   /* { dg-warning "nul past the end" } */
  T (4, "%hhi %hhi", 11, 12);   /* { dg-warning "into a region" } */

  T (5, "%0*hhd %0*hhi", 0,  7, 0,   9);
  T (5, "%0*hhd %0*hhi", 1,  7, 1,   9);
  T (5, "%0*hhd %0*hhi", 1,  7, 2,   9);
  T (5, "%0*hhd %0*hhi", 2,  7, 1,   9);
  T (5, "%0*hhd %0*hhi", 2,  7, 2,   9); /* { dg-warning "nul past the end" } */
  T (5, "%0*hhd %0*hhi", 0, 12, 0, 123); /* { dg-warning ".%0\\*hhi. directive writing 3 bytes into a region of size 2" } */
  T (5, "%0*hhd %0*hhi", 1, 12, 1, 123); /* { dg-warning ".%0\\*hhi. directive writing 3 bytes into a region of size 2" } */
  T (5, "%0*hhd %0*hhi", 2, 12, 3, 123); /* { dg-warning ".%0\\*hhi. directive writing 3 bytes into a region of size 2" } */

  /* FIXME: Move the boundary test cases into a file of their own that's
     exercised only on targets with the matching type limits (otherwise
     they'll fail).  */
#undef MAX
#define MAX   127

#undef MIN
#define MIN   (-MAX -1)

  T (1, "%hhi",        MAX);    /* { dg-warning "into a region" } */
  T (1, "%hhi",        MIN);    /* { dg-warning "into a region" } */
  T (1, "%hhi",  MAX +   1);    /* { dg-warning "into a region" } */

  T (2, "%hhi",  MAX +   1);    /* { dg-warning "into a region" } */
  T (2, "%hhi",  MAX +  10);    /* { dg-warning "into a region" } */
  T (2, "%hhi",  MAX + 100);    /* { dg-warning "into a region" } */
}

/* Exercise the "%hhd", "%hi", "%ho", "%hu", and "%hx" directives
   with constant arguments.  */

void test_sprintf_chk_h_const (void)
{
  T (1, "%hu",          0);     /* { dg-warning "nul past the end" } */
  T (1, "%hu",          1);     /* { dg-warning "nul past the end" } */
  T (1, "%hu",         -1);     /* { dg-warning "into a region" } */

  T (2, "%hi",          0);
  T (2, "%hi",          1);
  T (2, "%hi",          9);
  T (2, "% hi",         9);     /* { dg-warning "nul past the end" } */
  T (2, "%+hi",         9);     /* { dg-warning "nul past the end" } */
  T (2, "%-hi",         9);
  T (2, "%hi",         10);     /* { dg-warning "nul past the end" } */
  T (2, "%hi",         -1);     /* { dg-warning "nul past the end" } */
  T (2, "% hi",        -2);     /* { dg-warning "nul past the end" } */
  T (2, "%+hi",        -3);     /* { dg-warning "nul past the end" } */
  T (2, "%-hi",        -4);     /* { dg-warning "nul past the end" } */

  T (2, "%hu",          0);
  T (2, "%hu",          1);
  T (2, "%hu",          9);
  T (2, "%hu",         10);     /* { dg-warning "nul past the end" } */
  T (2, "%hu",         -1);     /* { dg-warning "into a region" } */

  T (2, "%ho",          0);
  T (2, "%ho",          1);
  T (2, "%ho",          7);
  T (2, "%ho",        010);     /* { dg-warning "nul past the end" } */
  T (2, "%ho",        077);     /* { dg-warning "nul past the end" } */
  T (2, "%ho",       0100);     /* { dg-warning "into a region" } */
  T (2, "%ho",         -1);     /* { dg-warning "into a region" } */

  T (2, "%hx",          0);
  T (2, "%hx",          1);
  T (2, "%hx",          7);
  T (2, "%hx",        0xf);
  T (2, "%hx",       0x10);     /* { dg-warning "nul past the end" } */
  T (2, "%hx",       0xff);     /* { dg-warning "nul past the end" } */
  T (2, "%hx",      0x100);     /* { dg-warning "into a region" } */
  T (2, "%hx",         -1);     /* { dg-warning "into a region" } */

  T (3, "% hi",         7);
  T (3, "%+hi",         8);
  T (3, "%-hi",         9);
  T (3, "%hi",         10);
  T (3, "%hi",         -1);
  T (3, "% hi",        -2);
  T (3, "%+hi",        -3);
  T (3, "%-hi",        -4);

  T (5, "%hu",       9999);
  T (5, "%hu",      10000);     /* { dg-warning "nul past the end" } */
  T (5, "%hu",      65535);     /* { dg-warning "nul past the end" } */

  T (1, "%#hx",         0);     /* { dg-warning "nul past the end" } */
  T (2, "%#hx",         0);
  T (3, "%#hx",         1);     /* { dg-warning "nul past the end" } */

  T (4, "%#hx",         0);
  T (4, "%#hx",         1);
  T (4, "%#hx",       0xf);
  T (4, "%#hx",      0x10);     /* { dg-warning "nul past the end" } */
  T (4, "%#hx",      0xff);     /* { dg-warning "nul past the end" } */
  T (4, "%#hx",     0x100);     /* { dg-warning "into a region" } */
  T (4, "%#hx",        -1);     /* { dg-warning "into a region" } */

#undef MAX
#define MAX   65535

  T (1, "%hhu",         0);     /* { dg-warning "nul past the end" } */
  T (1, "%hhu",         1);     /* { dg-warning "nul past the end" } */
  T (1, "%hhu",        -1);     /* { dg-warning "into a region" } */
  T (1, "%hhu",       MAX);     /* { dg-warning "into a region" } */
  T (1, "%hhu",  MAX +  1);     /* { dg-warning "nul past the end" } */
}

/* Exercise the "%d", "%i", "%o", "%u", and "%x" directives with
   constant arguments.  */

void test_sprintf_chk_integer_const (void)
{
  T ( 1, "%i",          0);         /* { dg-warning "nul past the end" } */
  T ( 1, "%i",          1);         /* { dg-warning "nul past the end" } */
  T ( 1, "%i",         -1);         /* { dg-warning "into a region" } */
  T ( 1, "%i_",         1);         /* { dg-warning "character ._. at offset 2 past the end" } */
  T ( 1, "_%i",         1);         /* { dg-warning "into a region" } */
  T ( 1, "_%i_",        1);         /* { dg-warning "into a region" } */
  T ( 1, "%o",          0);         /* { dg-warning "nul past the end" } */
  T ( 1, "%u",          0);         /* { dg-warning "nul past the end" } */
  T ( 1, "%x",          0);         /* { dg-warning "nul past the end" } */
  T ( 1, "%#x",         0);         /* { dg-warning "nul past the end" } */
  T ( 1, "%x",          1);         /* { dg-warning "nul past the end" } */
  T ( 1, "%#x",         1);         /* { dg-warning "into a region" } */

  T ( 2, "%i",          0);
  T ( 2, "%i",          1);
  T ( 2, "%i",          9);
  T ( 2, "%i",         -1);         /* { dg-warning "nul past the end" } */
  T ( 2, "%i",         10);         /* { dg-warning "nul past the end" } */
  T ( 2, "%i_",         0);         /* { dg-warning "nul past the end" } */
  T ( 2, "_%i",         0);         /* { dg-warning "nul past the end" } */
  T ( 2, "_%i_",        0);         /* { dg-warning "character ._. at offset 3 past the end" } */
  T ( 2, "%o",          1);
  T ( 2, "%o",          7);
  T ( 2, "%o",        010);         /* { dg-warning "nul past the end" } */
  T ( 2, "%o",       0100);         /* { dg-warning "into a region" } */
  T ( 2, "%x",          1);
  T ( 2, "%#x",         1);         /* { dg-warning "into a region" } */
  T ( 2, "%x",        0xa);
  T ( 2, "%x",        0xf);
  T ( 2, "%x",       0x10);         /* { dg-warning "nul past the end" } */
  T ( 2, "%x",       0xff);         /* { dg-warning "nul past the end" } */
  T ( 2, "%x",      0x1ff);         /* { dg-warning "into a region" } */

  T ( 3, "%i",          0);
  T ( 3, "%i",          1);
  T ( 3, "%i",          9);
  T ( 3, "%i",         -9);
  T ( 3, "%i",         10);
  T ( 3, "%i",         99);
  T ( 3, "%i",        -99);         /* { dg-warning "nul past the end" } */

  /* ~0U is formatted into exactly three bytes as "-1" followed by
     the terminating NUL character.  */
  T ( 3, "%+i",       ~0U);
  T ( 3, "%-i",       ~0U);
  T ( 3, "% i",       ~0U);

  T ( 8, "%8u",         1);        /* { dg-warning "nul past the end" } */
  T ( 9, "%8u",         1);

  T ( 7, "%1$i%2$i%3$i",     1, 23, 456);
  T ( 8, "%1$i%2$i%3$i%1$i", 1, 23, 456);
  T ( 8, "%1$i%2$i%3$i%2$i", 1, 23, 456);   /* { dg-warning "nul past the end" } */
  T ( 8, "%1$i%2$i%3$i%3$i", 1, 23, 456);   /* { dg-warning "into a region" } */

#undef MAX
#define MAX   2147483647   /* 10 digits.  */
#undef MIN
#define MIN   (-MAX -1)    /* Sign plus 10 digits.  */

  T ( 1, "%i",        MAX);         /* { dg-warning "into a region" } */
  T ( 1, "%i",        MIN);         /* { dg-warning "into a region" } */
  T ( 2, "%i",        MAX);         /* { dg-warning "into a region" } */
  T ( 2, "%i",        MIN);         /* { dg-warning "into a region" } */
  T (10, "%i",  123456789);
  T (10, "%i", -123456789);         /* { dg-warning "nul past the end" } */
  T (10, "%i",        MAX);         /* { dg-warning "nul past the end" } */
  T (10, "%i",        MIN);         /* { dg-warning "into a region" } */

  T (11, "%i",        MAX);
  T (11, "%i",        MIN);         /* { dg-warning "nul past the end" } */
}

/* Exercise the "%jd", "%ji", "%jo", "%ju", and "%jx" directives
   for the formatting of intmax_t and uintmax_t values with constant
   arguments.  */

void test_sprintf_chk_j_const (void)
{
#define I(x) ((__INTMAX_TYPE__)x)

  T ( 1, "%ji",  I (    0));      /* { dg-warning "nul past the end" } */
  T ( 1, "%ji",  I (    1));      /* { dg-warning "nul past the end" } */
  T ( 1, "%ji",  I (   -1));      /* { dg-warning "into a region" } */
  T ( 1, "%ji_", I (    1));      /* { dg-warning "character ._. at offset 3 past the end" } */
  T ( 1, "_%ji", I (    1));      /* { dg-warning "into a region" } */
  T ( 1, "_%ji_",I (    1));      /* { dg-warning "into a region" } */
  T ( 1, "%jo",  I (    0));      /* { dg-warning "nul past the end" } */
  T ( 1, "%ju",  I (    0));      /* { dg-warning "nul past the end" } */
  T ( 1, "%jx",  I (    0));      /* { dg-warning "nul past the end" } */
  T ( 1, "%#jx", I (    0));      /* { dg-warning "nul past the end" } */
  T ( 1, "%jx",  I (    1));      /* { dg-warning "nul past the end" } */
  T ( 1, "%#jx", I (    1));      /* { dg-warning "into a region" } */

  T ( 2, "%ji",  I (    0));
  T ( 2, "%ji",  I (    1));
  T ( 2, "%ji",  I (    9));
  T ( 2, "%ji",  I (   -1));      /* { dg-warning "nul past the end" } */
  T ( 2, "%ji",  I (   10));      /* { dg-warning "nul past the end" } */
  T ( 2, "%ji_", I (    0));      /* { dg-warning "nul past the end" } */
  T ( 2, "_%ji", I (    0));      /* { dg-warning "nul past the end" } */
  T ( 2, "_%ji_",I (    0));      /* { dg-warning "character ._. at offset 4 past the end" } */
  T ( 2, "%jo",  I (    1));
  T ( 2, "%jo",  I (    7));
  T ( 2, "%jo",  I (  010));      /* { dg-warning "nul past the end" } */
  T ( 2, "%jo",  I ( 0100));      /* { dg-warning "into a region" } */
  T ( 2, "%jx",  I (    1));
  T ( 2, "%#jx", I (    1));      /* { dg-warning "into a region" } */
  T ( 2, "%jx",  I (  0xa));
  T ( 2, "%jx",  I (  0xf));
  T ( 2, "%jx",  I ( 0x10));      /* { dg-warning "nul past the end" } */
  T ( 2, "%jx",  I ( 0xff));      /* { dg-warning "nul past the end" } */
  T ( 2, "%jx",  I (0x1ff));      /* { dg-warning "into a region" } */

  T ( 3, "%ji",  I (    0));
  T ( 3, "%ji",  I (    1));
  T ( 3, "%ji",  I (    9));
  T ( 3, "%ji",  I (   -9));
  T ( 3, "%ji",  I (   10));
  T ( 3, "%ji",  I (   99));
  T ( 3, "%ji",  I (  -99));      /* { dg-warning "nul past the end" } */

  /* ~0 is formatted into exactly three bytes as "-1" followed by
     the terminating NUL character.  */
  T ( 3, "%+ji",    ~I (0));
  T ( 3, "%-ji",    ~I (0));
  T ( 3, "% ji",    ~I (0));

  T ( 8, "%8ju",     I (1));      /* { dg-warning "nul past the end" } */
  T ( 9, "%8ju",     I (1));
}

/* Exercise the "%ld", "%li", "%lo", "%lu", and "%lx" directives
   with constant arguments.  */

void test_sprintf_chk_l_const (void)
{
  T ( 1, "%li",      0L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%li",      1L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%li",     -1L);         /* { dg-warning "into a region" } */
  T ( 1, "%li_",     1L);         /* { dg-warning "character ._. at offset 3 past the end" } */
  T ( 1, "_%li",     1L);         /* { dg-warning "into a region" } */
  T ( 1, "_%li_",    1L);         /* { dg-warning "into a region" } */
  T ( 1, "%lo",      0L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%lu",      0L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%lx",      0L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%#lx",     0L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%lx",      1L);         /* { dg-warning "nul past the end" } */
  T ( 1, "%#lx",     1L);         /* { dg-warning "into a region" } */

  T ( 2, "%li",      0L);
  T ( 2, "%li",      1L);
  T ( 2, "%li",      9L);
  T ( 2, "%li",     -1L);         /* { dg-warning "nul past the end" } */
  T ( 2, "%li",     10L);         /* { dg-warning "nul past the end" } */
  T ( 2, "%li_",     0L);         /* { dg-warning "nul past the end" } */
  T ( 2, "_%li",     0L);         /* { dg-warning "nul past the end" } */
  T ( 2, "_%li_",    0L);         /* { dg-warning "character ._. at offset 4 past the end" } */
  T ( 2, "%lo",      1L);
  T ( 2, "%lo",      7L);
  T ( 2, "%lo",    010L);         /* { dg-warning "nul past the end" } */
  T ( 2, "%lo",   0100L);         /* { dg-warning "into a region" } */
  T ( 2, "%lx",      1L);
  T ( 2, "%#lx",     1L);         /* { dg-warning "into a region" } */
  T ( 2, "%lx",    0xaL);
  T ( 2, "%lx",    0xfL);
  T ( 2, "%lx",   0x10L);         /* { dg-warning "nul past the end" } */
  T ( 2, "%lx",   0xffL);         /* { dg-warning "nul past the end" } */
  T ( 2, "%lx",  0x1ffL);         /* { dg-warning "into a region" } */

  T ( 3, "%li",      0L);
  T ( 3, "%li",      1L);
  T ( 3, "%li",      9L);
  T ( 3, "%li",     -9L);
  T ( 3, "%li",     10L);
  T ( 3, "%li",     99L);
  T ( 3, "%li",    -99L);         /* { dg-warning "nul past the end" } */

  /* ~0U is formatted into exactly three bytes as "-1" followed by
     the terminating NUL character.  */
  T ( 3, "%+li",   ~0LU);
  T ( 3, "%-li",   ~0LU);
  T ( 3, "% li",   ~0LU);

  T ( 8, "%8lu",     1L);         /* { dg-warning "nul past the end" } */
  T ( 9, "%8lu",     1L);
}

/* Exercise the "%lld", "%lli", "%llo", "%llu", and "%llx" directives
   with constant arguments.  */

void test_sprintf_chk_ll_const (void)
{
  T ( 1, "%lli",      0LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%lli",      1LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%lli",     -1LL);     /* { dg-warning "into a region" } */
  T ( 1, "%lli_",     1LL);     /* { dg-warning "character ._. at offset 4 past the end" } */
  T ( 1, "_%lli",     1LL);     /* { dg-warning "into a region" } */
  T ( 1, "_%lli_",    1LL);     /* { dg-warning "into a region" } */
  T ( 1, "%llo",      0LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%llu",      0LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%llx",      0LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%#llx",     0LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%llx",      1LL);     /* { dg-warning "nul past the end" } */
  T ( 1, "%#llx",     1LL);     /* { dg-warning "into a region" } */

  T ( 2, "%lli",      0LL);
  T ( 2, "%lli",      1LL);
  T ( 2, "%lli",      9LL);
  T ( 2, "%lli",     -1LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "%lli",     10LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "%lli_",     0LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "_%lli",     0LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "_%lli_",    0LL);     /* { dg-warning "character ._. at offset 5 past the end" } */
  T ( 2, "%llo",      1LL);
  T ( 2, "%llo",      7LL);
  T ( 2, "%llo",    010LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "%llo",   0100LL);     /* { dg-warning "into a region" } */
  T ( 2, "%llx",      1LL);
  T ( 2, "%#llx",     1LL);     /* { dg-warning "into a region" } */
  T ( 2, "%llx",    0xaLL);
  T ( 2, "%llx",    0xfLL);
  T ( 2, "%llx",   0x10LL);     /* { dg-warning "nul past the end" } */
  T ( 2, "%llx",   0xffLL);     /* { dg-warning "nul past the end" } */
  T ( 2, "%llx",  0x1ffLL);     /* { dg-warning "into a region" } */

  T ( 3, "%lli",      0LL);
  T ( 3, "%lli",      1LL);
  T ( 3, "%lli",      9LL);
  T ( 3, "%lli",     -9LL);
  T ( 3, "%lli",     10LL);
  T ( 3, "%lli",     99LL);
  T ( 3, "%lli",    -99LL);     /* { dg-warning "nul past the end" } */

  /* ~0U is formatted into exactly three bytes as "-1" followed by
     the terminating NUL character.  */
  T ( 3, "%+lli",   ~0LLU);
  T ( 3, "%-lli",   ~0LLU);
  T ( 3, "% lli",   ~0LLU);

  T ( 8, "%8llu",     1LL);     /* { dg-warning "nul past the end" } */
  T ( 9, "%8llu",     1LL);

  /* assume 64-bit long long.  */
#define LLONG_MAX   9223372036854775807LL   /* 19 bytes */
#define LLONG_MIN   (-LLONG_MAX - 1)        /* 20 bytes */

  T (18, "%lli", LLONG_MIN);    /* { dg-warning "into a region" } */
  T (19, "%lli", LLONG_MIN);    /* { dg-warning "into a region" } */
  T (20, "%lli", LLONG_MIN);    /* { dg-warning "nul past the end" } */
  T (21, "%lli", LLONG_MIN);

  T (18, "%lli", LLONG_MAX);    /* { dg-warning "into a region" } */
  T (19, "%lli", LLONG_MAX);    /* { dg-warning "nul past the end" } */
  T (20, "%lli", LLONG_MAX);

  T (21, "%llo",      -1LL);    /* { dg-warning "into a region" } */
  T (22, "%llo",      -1LL);    /* { dg-warning "nul past the end" } */
  T (23, "%llo",      -1LL);

  T (19, "%llu",      -1LL);    /* { dg-warning "into a region" } */
  T (20, "%llu",      -1LL);    /* { dg-warning "nul past the end" } */
  T (21, "%llu",      -1LL);

  T (15, "%llx",      -1LL);    /* { dg-warning "into a region" } */
  T (16, "%llx",      -1LL);    /* { dg-warning "nul past the end" } */
  T (17, "%llx",      -1LL);
}

void test_sprintf_chk_L_const (void)
{
  T (-1, "%Li",        0LL);
  T ( 1, "%Li",        0LL);         /* { dg-warning "nul past the end" } */
  T ( 1, "%Li",        1LL);         /* { dg-warning "nul past the end" } */
  T ( 1, "%Li",       -1LL);         /* { dg-warning "into a region" } */
  T ( 1, "%Li_",       1LL);         /* { dg-warning "character ._. at offset 3 past the end" } */
  T ( 1, "_%Li",       1LL);         /* { dg-warning "into a region" } */
  T ( 1, "_%Li_",      1LL);         /* { dg-warning "into a region" } */
}

void test_sprintf_chk_z_const (void)
{
  T (-1, "%zi",        (size_t)0);
  T ( 1, "%zi",        (size_t)0);  /* { dg-warning "nul past the end" } */
  T ( 1, "%zi",        (size_t)1);  /* { dg-warning "nul past the end" } */
  T ( 1, "%zi",        (size_t)-1L);/* { dg-warning "into a region" } */
  T ( 1, "%zi_",       (size_t)1);  /* { dg-warning "character ._. at offset 3 past the end" } */
  T ( 1, "_%zi",       (size_t)1);  /* { dg-warning "into a region" } */
  T ( 1, "_%zi_",      (size_t)1);  /* { dg-warning "into a region" } */

  T ( 2, "%zu",        (size_t)1);
  T ( 2, "%zu",        (size_t)9);
  T ( 2, "%zu",        (size_t)10); /* { dg-warning "nul past the end" } */
}

void test_sprintf_chk_e_const (void)
{
  T (-1, "%E",   0.0);
  T ( 0, "%E",   0.0);          /* { dg-warning "into a region" } */
  T ( 0, "%e",   0.0);          /* { dg-warning "into a region" } */
  T ( 1, "%E",   1.0);          /* { dg-warning "into a region" } */
  T ( 1, "%e",   1.0);          /* { dg-warning "into a region" } */
  T ( 2, "%e",   2.0);          /* { dg-warning "into a region" } */
  T ( 3, "%e",   3.0);          /* { dg-warning "into a region" } */
  T (12, "%e",   1.2);          /* { dg-warning "nul past the end" } */
  T (12, "%e",  12.0);          /* { dg-warning "nul past the end" } */
  T (13, "%e",   1.3);          /* 1.300000e+00 */
  T (13, "%E",  13.0);          /* 1.300000e+01 */
  T (13, "%e",  13.0);
  T (13, "%E",  1.4e+99);       /* 1.400000e+99 */
  T (13, "%e",  1.5e+100);      /* { dg-warning "nul past the end" } */
  T (14, "%E",  1.6e+101);      /* 1.600000E+101 */
  T (14, "%e", -1.7e+102);      /* { dg-warning "nul past the end" } */
  T (15, "%E", -1.8e+103);      /* -1.800000E+103 */

  T (16, "%.8e", -1.9e+104);    /* { dg-warning "nul past the end" } */
  T (17, "%.8e", -2.0e+105);    /* -2.00000000e+105 */

  T ( 5, "%.0e", 0.0);          /* { dg-warning "nul past the end" } */
  T ( 5, "%.0e", 1.0);          /* { dg-warning "nul past the end" } */
  T ( 6, "%.0e", 1.0);

  /* The actual output of the following directives depends on the rounding
     mode.  Verify that the warning correctly reflects that.  */
  T (12, "%e",  9.999999e+99);  /* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%e",  9.9999994e+99); /* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%e",  9.9999995e+99); /* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%e",  9.9999996e+99); /* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%e",  9.9999997e+99); /* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%e",  9.9999998e+99); /* { dg-warning "directive writing between 12 and 13 bytes" } */

  T (12, "%Le", 9.9999994e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%Le", 9.9999995e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%Le", 9.9999996e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%Le", 9.9999997e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%Le", 9.9999998e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
  T (12, "%Le", 9.9999999e+99L);/* { dg-warning "directive writing between 12 and 13 bytes" } */
}

/* At -Wformat-length level 1 unknown numbers are assumed to have
   the value one, and unknown strings are assumed to have a zero
   length.  */

void test_sprintf_chk_s_nonconst (int i, const char *s)
{
  T (-1, "%s",   s);
  T ( 0, "%s",   s);            /* { dg-warning "nul past the end" } */
  T ( 1, "%s",   s);
  T ( 1, "%.0s", s);
  T ( 1, "%.1s", s);            /* { dg-warning "nul past the end" } */

  /* The following will definitely write past the end of the buffer,
     but since at level 1 the length of an unknown string argument
     is assumed to be zero, it will write the terminating nul past
     the end (we don't print "past the end" when we're not
     sure which we can't be with an unknown string.  */
  T (1, "%1s",  s);             /* { dg-warning "writing a terminating nul past the end" } */
}

/* Exercise the hh length modifier with all integer specifiers and
   a non-constant argument.  */

void test_sprintf_chk_hh_nonconst (int a)
{
  T (-1, "%hhd",        a);

  T (0, "%hhd",         a);     /* { dg-warning "into a region" } */
  T (0, "%hhi",         a);     /* { dg-warning "into a region" } */
  T (0, "%hhu",         a);     /* { dg-warning "into a region" } */
  T (0, "%hhx",         a);     /* { dg-warning "into a region" } */

  T (1, "%hhd",         a);     /* { dg-warning "nul past the end" } */
  T (1, "%hhi",         a);     /* { dg-warning "nul past the end" } */
  T (1, "%hhu",         a);     /* { dg-warning "nul past the end" } */
  T (1, "%hhx",         a);     /* { dg-warning "nul past the end" } */

  T (1, "% hhd",        a);     /* { dg-warning "into a region" } */
  T (1, "% hhi",        a);     /* { dg-warning "into a region" } */
  T (1, "%+hhd",        a);     /* { dg-warning "into a region" } */
  T (1, "%+hhi",        a);     /* { dg-warning "into a region" } */
  T (1, "%-hhd",        a);     /* { dg-warning "nul past the end" } */
  T (1, "%-hhi",        a);     /* { dg-warning "nul past the end" } */

  T (2, "%hhd",         a);
  T (2, "%hhi",         a);
  T (2, "%hho",         a);
  T (2, "%hhu",         a);
  T (2, "%hhx",         a);

  T (2, "% hhd",        a);     /* { dg-warning "nul past the end" } */
  T (2, "% hhi",        a);     /* { dg-warning "nul past the end" } */
  T (2, "% hho",        a);     /* { dg-warning ". . flag used with .%o." } */
  T (2, "% hhu",        a);     /* { dg-warning ". . flag used with .%u." } */
  T (2, "% hhx",        a);     /* { dg-warning ". . flag used with .%x." } */

  T (2, "#%hho",        a);     /* { dg-warning "nul past the end" } */
  T (2, "#%hhx",        a);     /* { dg-warning "nul past the end" } */

  T (3, "%2hhd",        a);
  T (3, "%2hhi",        a);
  T (3, "%2hho",        a);
  T (3, "%2hhu",        a);
  T (3, "%2hhx",        a);

  /* Exercise cases where the type of the actual argument (whose value
     and range are unknown) constrain the size of the output and so
     can be used to avoid what would otherwise be false positives.  */

  T (2, "%hhd", (UChar)a);
  T (2, "%hhi", (UChar)a);
  T (2, "%-hhi", (UChar)a);
}

/* Exercise the h length modifier with all integer specifiers and
   a non-constant argument.  */

void test_sprintf_chk_h_nonconst (int a)
{
  T (-1, "%hd",         a);

  T (0, "%hd",          a);     /* { dg-warning "into a region" } */
  T (0, "%hi",          a);     /* { dg-warning "into a region" } */
  T (0, "%hu",          a);     /* { dg-warning "into a region" } */
  T (0, "%hx",          a);     /* { dg-warning "into a region" } */

  T (1, "%hd",          a);     /* { dg-warning "nul past the end" } */
  T (1, "%hi",          a);     /* { dg-warning "nul past the end" } */
  T (1, "%hu",          a);     /* { dg-warning "nul past the end" } */
  T (1, "%hx",          a);     /* { dg-warning "nul past the end" } */

  T (1, "% hd",         a);     /* { dg-warning "into a region" } */
  T (1, "% hi",         a);     /* { dg-warning "into a region" } */
  T (1, "%+hd",         a);     /* { dg-warning "into a region" } */
  T (1, "%+hi",         a);     /* { dg-warning "into a region" } */
  T (1, "%-hd",         a);     /* { dg-warning "nul past the end" } */
  T (1, "%-hi",         a);     /* { dg-warning "nul past the end" } */

  T (2, "%hd",          a);
  T (2, "%hi",          a);
  T (2, "%ho",          a);
  T (2, "%hu",          a);
  T (2, "%hx",          a);

  T (2, "% hd",         a);     /* { dg-warning "nul past the end" } */
  T (2, "% hi",         a);     /* { dg-warning "nul past the end" } */
  T (2, "% ho",         a);     /* { dg-warning ". . flag used with .%o." } */
  T (2, "% hu",         a);     /* { dg-warning ". . flag used with .%u." } */
  T (2, "% hx",         a);     /* { dg-warning ". . flag used with .%x." } */

  T (2, "#%ho",         a);     /* { dg-warning "nul past the end" } */
  T (2, "#%hx",         a);     /* { dg-warning "nul past the end" } */

  T (3, "%2hd",         a);
  T (3, "%2hi",         a);
  T (3, "%2ho",         a);
  T (3, "%2hu",         a);
  T (3, "%2hx",         a);
}

/* Exercise all integer specifiers with no modifier and a non-constant
   argument.  */

void test_sprintf_chk_int_nonconst (int a)
{
  T (-1, "%d",          a);

  T (0, "%d",           a);     /* { dg-warning "into a region" } */
  T (0, "%i",           a);     /* { dg-warning "into a region" } */
  T (0, "%u",           a);     /* { dg-warning "into a region" } */
  T (0, "%x",           a);     /* { dg-warning "into a region" } */

  T (1, "%d",           a);     /* { dg-warning "nul past the end" } */
  T (1, "%i",           a);     /* { dg-warning "nul past the end" } */
  T (1, "%u",           a);     /* { dg-warning "nul past the end" } */
  T (1, "%x",           a);     /* { dg-warning "nul past the end" } */

  T (1, "% d",          a);     /* { dg-warning "into a region" } */
  T (1, "% i",          a);     /* { dg-warning "into a region" } */
  T (1, "%+d",          a);     /* { dg-warning "into a region" } */
  T (1, "%+i",          a);     /* { dg-warning "into a region" } */
  T (1, "%-d",          a);     /* { dg-warning "nul past the end" } */
  T (1, "%-i",          a);     /* { dg-warning "nul past the end" } */

  T (2, "%d",           a);
  T (2, "%i",           a);
  T (2, "%o",           a);
  T (2, "%u",           a);
  T (2, "%x",           a);

  T (2, "% d",          a);     /* { dg-warning "nul past the end" } */
  T (2, "% i",          a);     /* { dg-warning "nul past the end" } */
  T (2, "% o",          a);     /* { dg-warning ". . flag used with .%o." } */
  T (2, "% u",          a);     /* { dg-warning ". . flag used with .%u." } */
  T (2, "% x",          a);     /* { dg-warning ". . flag used with .%x." } */

  T (2, "#%o",          a);     /* { dg-warning "nul past the end" } */
  T (2, "#%x",          a);     /* { dg-warning "nul past the end" } */

  T (3, "%2d",          a);
  T (3, "%2i",          a);
  T (3, "%2o",          a);
  T (3, "%2u",          a);
  T (3, "%2x",          a);
}

void test_sprintf_chk_e_nonconst (double d)
{
  T (-1, "%E",          d);
  T ( 0, "%E",          d);           /* { dg-warning "writing between 12 and 14 bytes into a region of size 0" } */
  T ( 0, "%e",          d);           /* { dg-warning "into a region" } */
  T ( 1, "%E",          d);           /* { dg-warning "into a region" } */
  T ( 1, "%e",          d);           /* { dg-warning "into a region" } */
  T ( 2, "%e",          d);           /* { dg-warning "into a region" } */
  T ( 3, "%e",          d);           /* { dg-warning "into a region" } */
  T (12, "%e",          d);           /* { dg-warning "past the end" } */
  T (12, "%e",          d);           /* { dg-warning "past the end" } */
  T (13, "%E",          d);           /* 1.000000E+00 */
  T (13, "%e",          d);
  T (14, "%E",          d);
  T (14, "%e",          d);

  T  (0, "%+E",         d);           /* { dg-warning "writing between 13 and 14 bytes into a region of size 0" } */
  T  (0, "%-e",         d);           /* { dg-warning "writing between 12 and 14 bytes into a region of size 0" } */
  T  (0, "% E",         d);           /* { dg-warning "writing between 13 and 14 bytes into a region of size 0" } */

  /* The range of output of "%.0e" is between 5 and 7 bytes (not counting
     the terminating NUL.  */
  T ( 5, "%.0e",        d);           /* { dg-warning "writing a terminating nul past the end" } */
  T ( 6, "%.0e",        d);           /* 1e+00 */

  /* The range of output of "%.1e" is between 7 and 9 bytes (not counting
     the terminating NUL.  */
  T ( 7, "%.1e",        d);           /* { dg-warning "writing a terminating nul past the end" } */
  T ( 8, "%.1e",        d);
}

void test_sprintf_chk_f_nonconst (double d)
{
  T (-1, "%F",          d);
  T ( 0, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 0, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 1, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 1, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 2, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 2, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 3, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 3, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 4, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 4, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 5, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 5, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 6, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 6, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 7, "%F",          d);           /* { dg-warning "into a region" } */
  T ( 7, "%f",          d);           /* { dg-warning "into a region" } */
  T ( 8, "%F",          d);           /* { dg-warning "nul past the end" } */
  T ( 8, "%f",          d);           /* { dg-warning "nul past the end" } */
  T ( 9, "%F",          d);
  T ( 9, "%f",          d);
}

/* Tests for __builtin_vsprintf_chk are the same as those for
   __builtin_sprintf_chk with non-constant arguments.  */
#undef T
#define T(size, fmt)							\
  __builtin___vsprintf_chk (buffer (size), 0, objsize (size), fmt, va)

void test_vsprintf_chk_c (__builtin_va_list va)
{
  T (-1, "%c");

  /* Verify the full text of the diagnostic for just the distinct messages
     and use abbreviations in subsequent test cases.  */
  T (0, "%c");              /* { dg-warning ".%c. directive writing 1 byte into a region of size 0" } */
  T (1, "%c");              /* { dg-warning "writing a terminating nul past the end" } */
  T (1, "%c");              /* { dg-warning "nul past the end" } */
  T (2, "%c");
  T (2, "%2c");             /* { dg-warning "nul past the end" } */
  T (2, "%3c");             /* { dg-warning "into a region" } */
  T (2, "%c%c");            /* { dg-warning "nul past the end" } */
  T (3, "%c%c");

  /* Wide characters.  */
  T (0, "%lc");             /* { dg-warning "nul past the end" } */
  T (1, "%lc");
  T (2, "%lc");

  /* The following could result in as few as a single byte and in as many
     as MB_CUR_MAX, but since the MB_CUR_MAX value is a runtime property
     the write cannot be reliably diagnosed.  */
  T (2, "%lc");
  T (2, "%1lc");
  /* Writing some unknown number of bytes into a field two characters wide.  */
  T (2, "%2lc");            /* { dg-warning "nul past the end" } */
  T (2, "%lc%lc");

  T (3, "%lc%c");
  /* Here in the best case each argument will format as single character,
     causing the terminating NUL to be written past the end.  */
  T (3, "%lc%c%c");

}

void test_vsprintf_chk_int (__builtin_va_list va)
{
  T (-1, "%d");

  T (0, "%d");                /* { dg-warning "into a region" } */
  T (0, "%i");                /* { dg-warning "into a region" } */
  T (0, "%u");                /* { dg-warning "into a region" } */
  T (0, "%x");                /* { dg-warning "into a region" } */

  T (1, "%d");                /* { dg-warning "nul past the end" } */
  T (1, "%i");                /* { dg-warning "nul past the end" } */
  T (1, "%u");                /* { dg-warning "nul past the end" } */
  T (1, "%x");                /* { dg-warning "nul past the end" } */

  T (1, "% d");               /* { dg-warning "into a region" } */
  T (1, "% i");               /* { dg-warning "into a region" } */
  T (1, "%+d");               /* { dg-warning "into a region" } */
  T (1, "%+i");               /* { dg-warning "into a region" } */
  T (1, "%-d");               /* { dg-warning "nul past the end" } */
  T (1, "%-i");               /* { dg-warning "nul past the end" } */

  T (2, "%d");
  T (2, "%i");
  T (2, "%o");
  T (2, "%u");
  T (2, "%x");

  T (2, "% d");               /* { dg-warning "nul past the end" } */
  T (2, "% i");               /* { dg-warning "nul past the end" } */
  T (2, "% o");               /* { dg-warning ". . flag used with .%o." } */
  T (2, "% u");               /* { dg-warning ". . flag used with .%u." } */
  T (2, "% x");               /* { dg-warning ". . flag used with .%x." } */

  T (2, "#%o");               /* { dg-warning "nul past the end" } */
  T (2, "#%x");               /* { dg-warning "nul past the end" } */

  T (3, "%2d");
  T (3, "%2i");
  T (3, "%2o");
  T (3, "%2u");
  T (3, "%2x");
}

#undef T
#define T(size, fmt, ...)						\
  __builtin_snprintf (buffer (size), objsize (size), fmt, __VA_ARGS__)

void test_snprintf_c_const (void)
{
  T (-1, "%c",    0);            /* { dg-warning "specified destination size \[0-9\]+ too large" } */

  /* Verify the full text of the diagnostic for just the distinct messages
     and use abbreviations in subsequent test cases.  */

  /* A call to snprintf with a buffer of zero size is a request to determine
     the size of output without writing anything into the destination. No
     warning must be issued.  */
  T (0, "%c",     0);
  T (1, "%c",     0);            /* { dg-warning "output truncated before the last format character" } */
  T (1, "%c",   '1');            /* { dg-warning "output truncated" } */
  T (2, "%c",   '1');
  T (2, "%2c",  '1');            /* { dg-warning "output truncated" } */
  T (2, "%3c",  '1');            /* { dg-warning "directive output truncated" } */
  T (2, "%c%c", '1', '2');       /* { dg-warning "output truncated" } */
  T (3, "%c%c", '1', '2');

  /* Wide characters.  */
  T (0, "%lc",  0);
  T (1, "%lc",  0);
  T (2, "%lc",  0);

  /* The following could result in as few as a single byte and in as many
     as MB_CUR_MAX, but since the MB_CUR_MAX value is a runtime property
     the write cannot be reliably diagnosed.  */
  T (2, "%lc",  (wint_t)L'1');
  T (2, "%1lc", (wint_t)L'1');
  /* Writing at least 1 characted into a field two characters wide.  */
  T (2, "%2lc", (wint_t)L'1');          /* { dg-warning "output truncated before the last format character" } */

  T (3, "%lc%c",   (wint_t)'1', '2');
  /* Here in the best case each argument will format as single character,
     causing the output to be truncated just before the terminating NUL
     (i.e., cutting off the '3').  */
  T (3, "%lc%c%c", (wint_t)'1', '2', '3');   /* { dg-warning "output truncated" } */
  T (3, "%lc%lc%c", (wint_t)'1', (wint_t)'2', '3'); /* { dg-warning "output truncated" } */
}

#undef T
#define T(size, fmt, ...)						\
  __builtin___snprintf_chk (buffer (size), objsize (size),		\
			    0, objsize (size), fmt, __VA_ARGS__)

void test_snprintf_chk_c_const (void)
{
  /* Verify that specifying a size of the destination buffer that's
     bigger than its actual size (normally determined and passed to
     the function by __builtin_object_size) is diagnosed.  */
  __builtin___snprintf_chk (buffer, 3, 0, 2, " ");   /* { dg-warning "always overflow|specified size 3 exceeds the size 2 of the destination" } */

  T (-1, "%c",    0);           /* { dg-warning "specified destination size \[^ \]* too large" } */

  T (0, "%c",     0);
  T (0, "%c%c",   0, 0);
  T (0, "%c_%c",  0, 0);
  T (0, "_%c_%c", 0, 0);

  T (1, "%c",     0);            /* { dg-warning "output truncated before the last format character" } */
  T (1, "%c",   '1');            /* { dg-warning "output truncated" } */
  T (2, "%c",   '1');
  T (2, "%2c",  '1');            /* { dg-warning "output truncated" } */
  T (2, "%3c",  '1');            /* { dg-warning "directive output truncated" } */
  T (2, "%c%c", '1', '2');       /* { dg-warning "output truncated before the last format character" } */
  T (3, "%c%c", '1', '2');
  T (3, "%c_%c", '1', '2');      /* { dg-warning "output truncated" } */

  /* Wide characters.  */
  T (0, "%lc",  0);
  T (1, "%lc",  0);
  T (2, "%lc",  0);

  /* The following could result in as few as a single byte and in as many
     as MB_CUR_MAX, but since the MB_CUR_MAX value is a runtime property
     the write cannot be reliably diagnosed.  */
  T (2, "%lc",  (wint_t)L'1');
  T (2, "%1lc", (wint_t)L'1');
  /* Writing at least 1 characted into a field two characters wide.  */
  T (2, "%2lc", (wint_t)'1');          /* { dg-warning "output truncated before the last format character" } */

  T (3, "%lc%c",   (wint_t)'1', '2');
  /* Here in the best case each argument will format as single character,
     causing the output to be truncated just before the terminating NUL
     (i.e., cutting off the '3').  */
  T (3, "%lc%c%c", (wint_t)'1', '2', '3');   /* { dg-warning "output truncated" } */
  T (3, "%lc%lc%c", (wint_t)'1', (wint_t)'2', '3'); /* { dg-warning "output truncated" } */
}

/* Macro to verify that calls to __builtin_vsprintf (i.e., with no size
   argument) issue diagnostics by correctly determining the size of
   the destination buffer.  */
#undef T
#define T(size, fmt)				\
  __builtin_vsprintf (buffer (size), fmt, va)

void test_vsprintf_s (__builtin_va_list va)
{
  T (-1, "%s");

  T (0, "%s");              /* { dg-warning "writing a terminating nul past the end" } */
  T (1, "%s");
  T (1, "%1s");             /* { dg-warning "writing a terminating nul past the end" } */

  T (2, "%s%s");
  T (2, "%s%s_");
  T (2, "%s_%s");
  T (2, "_%s%s");
  T (2, "_%s_%s");          /* { dg-warning "writing a terminating nul past the end" } */
}

/* Exercise all integer specifiers with no modifier and a non-constant
   argument.  */

void test_vsprintf_int (__builtin_va_list va)
{
  T (-1, "%d");

  T (0, "%d");     /* { dg-warning "into a region" } */
  T (0, "%i");     /* { dg-warning "into a region" } */
  T (0, "%u");     /* { dg-warning "into a region" } */
  T (0, "%x");     /* { dg-warning "into a region" } */

  T (1, "%d");     /* { dg-warning "nul past the end" } */
  T (1, "%i");     /* { dg-warning "nul past the end" } */
  T (1, "%u");     /* { dg-warning "nul past the end" } */
  T (1, "%x");     /* { dg-warning "nul past the end" } */

  T (1, "% d");     /* { dg-warning "into a region" } */
  T (1, "% i");     /* { dg-warning "into a region" } */
  T (1, "%+d");     /* { dg-warning "into a region" } */
  T (1, "%+i");     /* { dg-warning "into a region" } */
  T (1, "%-d");     /* { dg-warning "nul past the end" } */
  T (1, "%-i");     /* { dg-warning "nul past the end" } */

  T (2, "%d");
  T (2, "%i");
  T (2, "%o");
  T (2, "%u");
  T (2, "%x");

  T (2, "% d");     /* { dg-warning "nul past the end" } */
  T (2, "% i");     /* { dg-warning "nul past the end" } */
  T (2, "% o");     /* { dg-warning ". . flag used with .%o." } */
  T (2, "% u");     /* { dg-warning ". . flag used with .%u." } */
  T (2, "% x");     /* { dg-warning ". . flag used with .%x." } */

  T (2, "#%o");     /* { dg-warning "nul past the end" } */
  T (2, "#%x");     /* { dg-warning "nul past the end" } */

  T (3, "%2d");
  T (3, "%2i");
  T (3, "%2o");
  T (3, "%2u");
  T (3, "%2x");
}

#undef T
#define T(size, fmt)							\
  __builtin_vsnprintf (buffer (size), objsize (size), fmt, va)

void test_vsnprintf_s (__builtin_va_list va)
{
  T (-1, "%s");             /* { dg-warning "specified destination size \[^ \]* too large" } */

  T (0, "%s");
  T (1, "%s");
  T (1, "%1s");             /* { dg-warning "output truncated before the last format character" } */

  T (2, "%s%s");
  T (2, "%s%s_");
  T (2, "%s_%s");
  T (2, "_%s%s");
  T (2, "_%s_%s");          /* { dg-warning "output truncated before the last format character" } */
}

#undef T
#define T(size, fmt)							\
  __builtin___vsnprintf_chk (buffer (size), objsize (size),		\
			     0, objsize (size), fmt, va)

void test_vsnprintf_chk_s (__builtin_va_list va)
{
  /* Verify that specifying a size of the destination buffer that's
     bigger than its actual size (normally determined and passed to
     the function by __builtin_object_size) is diagnosed.  */
  __builtin___snprintf_chk (buffer, 123, 0, 122, " ");   /* { dg-warning "always overflow|specified size 123 exceeds the size 122 of the destination object" } */

  __builtin___snprintf_chk (buffer, __SIZE_MAX__, 0, 2, " ");   /* { dg-warning "always overflow|destination size .\[0-9\]+. too large" } */

  T (0, "%s");
  T (1, "%s");
  T (1, "%1s");             /* { dg-warning "output truncated before the last format character" } */

  T (2, "%s%s");
  T (2, "%s%s_");
  T (2, "%s_%s");
  T (2, "_%s%s");
  T (2, "_%s_%s");          /* { dg-warning "output truncated before the last format character" } */
}
