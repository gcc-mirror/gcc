/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab" } */

#include <stdarg.h>

extern int vprintf (const char *, va_list);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9, via;

void
test (va_list ap1, va_list ap2, va_list ap3, va_list ap4, va_list ap5,
      va_list ap6, va_list ap7)
{
  vi0 = 0;
  vprintf ("hello", ap1);
  vi1 = 0;
  vprintf ("hello\n", ap2);
  vi2 = 0;
  vprintf ("a", ap3);
  vi3 = 0;
  vprintf ("", ap4);
  vi4 = 0;
  vprintf ("%s", ap5);
  vi5 = 0;
  vprintf ("%c", ap6);
  vi6 = 0;
  vprintf ("%s\n", ap7);
  vi7 = 0;
}

/* { dg-final { scan-tree-dump "vi0.*vprintf.*\"hello\".*vi1" "fab"} } */
/* { dg-final { scan-tree-dump "vi1.*puts.*\"hello\".*vi2" "fab"} } */
/* { dg-final { scan-tree-dump "vi2.*putchar.*vi3" "fab"} } */
/* { dg-final { scan-tree-dump "vi3 ={v} 0\[^\(\)\]*vi4 ={v} 0" "fab"} } */
/* { dg-final { scan-tree-dump "vi4.*vprintf.*\"%s\".*vi5" "fab"} } */
/* { dg-final { scan-tree-dump "vi5.*vprintf.*\"%c\".*vi6" "fab"} } */
/* { dg-final { scan-tree-dump "vi6.*vprintf.*\"%s\\\\n\".*vi7" "fab"} } */
/* { dg-final { cleanup-tree-dump "fab" } } */
