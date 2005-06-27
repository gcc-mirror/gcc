/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab" } */

#include <stdarg.h>

typedef struct { int i; } FILE;
FILE *fp;
extern int vfprintf (FILE *, const char *, va_list);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9, via;

void
test (va_list ap1, va_list ap2, va_list ap3, va_list ap4, va_list ap5,
      va_list ap6, va_list ap7)
{
  vi0 = 0;
  vfprintf (fp, "hello", ap1);
  vi1 = 0;
  vfprintf (fp, "hello\n", ap2);
  vi2 = 0;
  vfprintf (fp, "a", ap3);
  vi3 = 0;
  vfprintf (fp, "", ap4);
  vi4 = 0;
  vfprintf (fp, "%s", ap5);
  vi5 = 0;
  vfprintf (fp, "%c", ap6);
  vi6 = 0;
  vfprintf (fp, "%s\n", ap7);
  vi7 = 0;
}

/* { dg-final { scan-tree-dump "vi0.*fwrite.*\"hello\".*1, 5, fp.*vi1" "fab"} } */
/* { dg-final { scan-tree-dump "vi1.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi2" "fab"} } */
/* { dg-final { scan-tree-dump "vi2.*fputc.*fp.*vi3" "fab"} } */
/* { dg-final { scan-tree-dump "vi3 = 0\[^\(\)\]*vi4 = 0" "fab"} } */
/* { dg-final { scan-tree-dump "vi4.*vfprintf.*\"%s\".*vi5" "fab"} } */
/* { dg-final { scan-tree-dump "vi5.*vfprintf.*\"%c\".*vi6" "fab"} } */
/* { dg-final { scan-tree-dump "vi6.*vfprintf.*\"%s\\\\n\".*vi7" "fab"} } */
