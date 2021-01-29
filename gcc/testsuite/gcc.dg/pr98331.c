/* PR debug/98331 */
/* { dg-do compile } */
/* { dg-options "-g -O2 -fcompare-debug" } */
/* { dg-additional-options "-march=x86-64" { target { i?86-*-* x86_64-*-* } } } */

void bar (const char *);
unsigned long long x;

void
foo (void)
{
  int a = 1;
  bar ("foo");
  int b = 2;
  __atomic_fetch_add (&x, 1, 0);
  int c = 3;
  __builtin_unreachable ();
}
