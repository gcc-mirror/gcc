/* PR tree-optimization/86552 - missing warning for reading past the end
   of non-string arrays
   Exercise non-string detection in sprintf.
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds -Wall -ftrack-macro-expansion=0" } */

#include "range.h"

typedef __WCHAR_TYPE__ wchar_t;

extern int sprintf (char*, const char*, ...);

extern char *dst;

int i0 = 0;
int i1 = 1;

void sink (int, ...);

#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT(a, b)

#define T(fmt, ...)				\
  sink (sprintf (dst, fmt, __VA_ARGS__))

const char a[5] = "12345";    /* { dg-message "declared here" } */
const char b[6] = "123456";   /* { dg-message "declared here" } */
const char a2[][3] = {
  "", "1", "12", "123", "123\000"   /* { dg-warning "initializer-string for array of chars is too long" } */
};


void test_narrow (void)
{
  /* Verify that precision suppresses the warning when it's less
     than the size of the array.  */
  T ("%.0s%.1s%.2s%.3s%.4s%.5s", a, a, a, a, a, a);

  T ("%s", a);          /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */
  T ("%.6s", a);        /* { dg-warning ".%.6s. directive argument is not a nul-terminated string" } */

  /* Exercise conditional expressions involving strings and non-strings.  */
  const char *s0 = i0 < 0 ? a2[0] : a2[3];
  T ("%s", s0);         /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */
  s0 = i0 < 0 ? "123456" : a2[4];
  T ("%s", s0);         /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */

  const char *s1 = i0 < 0 ? a2[3] : a2[0];
  T ("%s", s1);         /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */

  const char *s2 = i0 < 0 ? a2[3] : a2[4];
  T ("%s", s2);         /* { dg-warning ".%s. directive argument is not a nul-terminated string" } */

  s0 = i0 < 0 ? a : b;
  T ("%.5s", s0);

  /* Verify that the warning triggers even if precision prevents
     reading past the end of one of the non-terminated arrays but
     not the other.  */
  T ("%.6s", s0);       /* { dg-warning ".%.6s. directive argument is not a nul-terminated string" } */

  s0 = i0 < 0 ? b : a;
  T ("%.7s", s0);       /* { dg-warning ".%.7s. directive argument is not a nul-terminated string" } */

  /* Verify that at -Wformat-overflow=1 the lower bound of precision
     given by a range is used to determine whether or not to warn.  */
  int r = SR (4, 5);

  T ("%.*s", r, a);
  T ("%.*s", r, b);

  r = SR (5, 6);
  T ("%.*s", r, a);
  T ("%.*s", r, b);

  r = SR (6, 7);
  T ("%.*s", r, a);     /* { dg-warning ".%.\\\*s. directive argument is not a nul-terminated string" } */
  T ("%.*s", r, b);
}


const wchar_t wa[5] = L"12345";   /* { dg-message "declared here" } */

void test_wide (void)
{
  T ("%.0ls%.1ls%.2ls%.3ls%.4ls%.5ls", wa, wa, wa, wa, wa, wa);

  T ("%ls", wa);        /* { dg-warning ".%ls. directive argument is not a nul-terminated string" } */
  T ("%.6ls", wa);      /* { dg-warning ".%.6ls. directive argument is not a nul-terminated string" } */
}
