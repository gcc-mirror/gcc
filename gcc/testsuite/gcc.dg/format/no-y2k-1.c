/* Test for warnings for Y2K problems being disabled by -Wno-format-y2k.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wno-format-y2k" } */

typedef __SIZE_TYPE__ size_t;

struct tm;

extern size_t strftime (char *, size_t, const char *, const struct tm *);

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%y%c%x", tp);
}
