/* Test for asm_fprintf formats.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

/* Magic identifier must be set before the attribute is used.  */
typedef long long __gcc_host_wide_int__;

extern int asm_fprintf (const char *, ...) __attribute__ ((__format__ (__asm_fprintf__, 1, 2))) __attribute__ ((__nonnull__));

void
foo (int i, int i1, int i2, unsigned int u, double d, char *s, void *p,
     int *n, short int *hn, long int l, unsigned long int ul,
     long int *ln, long double ld, wint_t lc, wchar_t *ls, llong ll,
     ullong ull, unsigned int *un, const int *cn, signed char *ss,
     unsigned char *us, const signed char *css, unsigned int u1,
     unsigned int u2)
{
  /* Acceptable C90 specifiers, flags and modifiers.  */
  asm_fprintf ("%%");
  asm_fprintf ("%d%i%o%u%x%X%c%s%%", i, i, u, u, u, u, i, s);
  asm_fprintf ("%ld%li%lo%lu%lx%lX", l, l, ul, ul, ul, ul);
  asm_fprintf ("%lld%lli%llo%llu%llx%llX", ll, ll, ull, ull, ull, ull);
  asm_fprintf ("%-d%-i%-o%-u%-x%-X%-c%-s", i, i, u, u, u, u, i, s);
  asm_fprintf ("% d% i\n", i, i);
  asm_fprintf ("%#o%#x%#X", u, u, u);
  asm_fprintf ("%08d%08i%08o%08u%08x%08X", i, i, u, u, u, u);
  asm_fprintf ("%d\n", i);
  asm_fprintf ("%+d\n", i);
  asm_fprintf ("%3d\n", i);
  asm_fprintf ("%-3d\n", i);
  asm_fprintf ("%.7d\n", i);
  asm_fprintf ("%+9.4d\n", i);
  asm_fprintf ("%.3ld\n", l);
  asm_fprintf ("%d %lu\n", i, ul);

  /* Extensions provided in asm_fprintf.  */
  asm_fprintf ("%O%R%I%L%U%@");
  asm_fprintf ("%r", i);
  asm_fprintf ("%wd%wi%wo%wu%wx%wX", ll, ll, ull, ull, ull, ull);

  /* Standard specifiers not accepted in asm_fprintf.  */
  asm_fprintf ("%f\n", d); /* { dg-warning "18:format" "float" } */
  asm_fprintf ("%e\n", d); /* { dg-warning "18:format" "float" } */
  asm_fprintf ("%E\n", d); /* { dg-warning "18:format" "float" } */
  asm_fprintf ("%g\n", d); /* { dg-warning "18:format" "float" } */
  asm_fprintf ("%G\n", d); /* { dg-warning "18:format" "float" } */
  asm_fprintf ("%p\n", p); /* { dg-warning "18:format" "pointer" } */
  asm_fprintf ("%n\n", n); /* { dg-warning "18:format" "counter" } */
  asm_fprintf ("%hd\n", i); /* { dg-warning "18:format" "conversion" } */

  /* Various tests of bad argument types.  */
  asm_fprintf ("%d", l); /* { dg-warning "18:format" "bad argument types" } */
  asm_fprintf ("%wd", l); /* { dg-warning "19:format" "bad argument types" } */
  asm_fprintf ("%d", ll); /* { dg-warning "18:format" "bad argument types" } */
  asm_fprintf ("%*d\n", i1, i); /* { dg-warning "18:format" "bad * argument types" } */
  asm_fprintf ("%.*d\n", i2, i); /* { dg-warning "19:format" "bad * argument types" } */
  asm_fprintf ("%*.*ld\n", i1, i2, l); /* { dg-warning "18:format" "bad * argument types" } */
  asm_fprintf ("%ld", i); /* { dg-warning "19:format" "bad argument types" } */
  asm_fprintf ("%s", n); /* { dg-warning "18:format" "bad argument types" } */

  /* Wrong number of arguments.  */
  asm_fprintf ("%d%d", i); /* { dg-warning "20:matching" "wrong number of args" } */
  asm_fprintf ("%d", i, i); /* { dg-warning "16:arguments" "wrong number of args" } */
  /* Miscellaneous bogus constructions.  */
  asm_fprintf (""); /* { dg-warning "16:zero-length" "warning for empty format" } */
  asm_fprintf ("\0"); /* { dg-warning "18:embedded" "warning for embedded NUL" } */
  asm_fprintf ("%d\0", i); /* { dg-warning "20:embedded" "warning for embedded NUL" } */
  asm_fprintf ("%d\0%d", i, i); /* { dg-warning "20:embedded|too many" "warning for embedded NUL" } */
  asm_fprintf (NULL); /* { dg-warning "null" "null format string warning" } */
  asm_fprintf ("%"); /* { dg-warning "17:trailing" "trailing % warning" } */
  asm_fprintf ("%++d", i); /* { dg-warning "19:repeated" "repeated flag warning" } */
  asm_fprintf ((const char *)L"foo"); /* { dg-warning "30:wide" "wide string" } */
  asm_fprintf ("%s", (char *)0); /* { dg-warning "null" "%s with NULL" } */

  /* Make sure we still get warnings for regular printf.  */
  printf ("%d\n", ll); /* { dg-warning "13:format" "bad argument types" } */
}
/* { dg-warning "16:too many arguments for format" "too many arguments" { target *-*-* } 0 } */
