/* Test for warnings for non-string-literal formats.  Test for strftime formats.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-nonliteral" } */

typedef __SIZE_TYPE__ size_t;

struct tm;

extern size_t strftime (char *, size_t, const char *, const struct tm *);

void
foo (char *s, size_t m, const struct tm *tp, char *fmt)
{
  strftime (s, m, fmt, tp); /* { dg-warning "format string" "non-literal" } */
}
