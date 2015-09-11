/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab1" } */

#include <stdarg.h>

typedef struct { int i; } FILE;
FILE *fp;
extern int __vfprintf_chk (FILE *, int, const char *, va_list);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9, via;

void
test (va_list ap1, va_list ap2, va_list ap3, va_list ap4, va_list ap5,
      va_list ap6, va_list ap7)
{
  vi0 = 0;
  __vfprintf_chk (fp, 1, "hello", ap1);
  vi1 = 0;
  __vfprintf_chk (fp, 1, "hello\n", ap2);
  vi2 = 0;
  __vfprintf_chk (fp, 1, "a", ap3);
  vi3 = 0;
  __vfprintf_chk (fp, 1, "", ap4);
  vi4 = 0;
  __vfprintf_chk (fp, 1, "%s", ap5);
  vi5 = 0;
  __vfprintf_chk (fp, 1, "%c", ap6);
  vi6 = 0;
  __vfprintf_chk (fp, 1, "%s\n", ap7);
  vi7 = 0;
}

/* { dg-final { scan-tree-dump "vi0.*fwrite.*\"hello\".*1, 5, fp.*vi1" "fab1"} } */
/* { dg-final { scan-tree-dump "vi1.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi2" "fab1"} } */
/* { dg-final { scan-tree-dump "vi2.*fputc.*fp.*vi3" "fab1"} } */
/* { dg-final { scan-tree-dump "vi3 ={v} 0\[^\(\)\]*vi4 ={v} 0" "fab1"} } */
/* { dg-final { scan-tree-dump "vi4.*__vfprintf_chk.*fp.*1.*\"%s\".*vi5" "fab1"} } */
/* { dg-final { scan-tree-dump "vi5.*__vfprintf_chk.*fp.*1.*\"%c\".*vi6" "fab1"} } */
/* { dg-final { scan-tree-dump "vi6.*__vfprintf_chk.*fp.*1.*\"%s\\\\n\".*vi7" "fab1"} } */
