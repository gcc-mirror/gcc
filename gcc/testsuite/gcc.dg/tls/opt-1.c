/* { dg-do compile } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-options "-O2 -fPIC -mtune=i686" { target { i?86-*-* && ilp32 } } } */
/* { dg-options "-O2 -fPIC -mtune=i686" { target { x86_64-*-* && ilp32 } } } */

extern __thread int thr;

static int x;

static void
bar (void)
{
  x = 1;
}

static void
#ifdef __i386__
__attribute__ ((regparm (3)))
#endif
foo (const char *x, void *y, int *z)
{
  bar ();
}

void
test (const char *x, void *y)
{
  foo (x, y, &thr);
}
