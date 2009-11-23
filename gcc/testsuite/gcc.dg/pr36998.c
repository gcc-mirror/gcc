/* PR rtl-optimization/36998 */
/* { dg-do compile } */
/* { dg-options "-Os -fasynchronous-unwind-tables" } */
/* { dg-options "-Os -mpreferred-stack-boundary=2 -fasynchronous-unwind-tables" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-fno-omit-frame-pointer" { target { avr-*-* } } } */

void foo (const char *, ...) __attribute__ ((noreturn));
int bar (const char *, ...);
extern __SIZE_TYPE__ strlen (const char *);
int baz (char *, char *, int, void *);

void
test (char *w, int x, char *y, char *z)
{
  char *p, b[32];
  for (p = y; *p; p += strlen (p) + 1)
    {
      baz (w, p, x, z);
      foo ("msg1 %s", b);
    }
  for (p = y; *p; p += strlen (p) + 1)
    bar (" %s", p);
  foo ("msg2 %s", b);
}
