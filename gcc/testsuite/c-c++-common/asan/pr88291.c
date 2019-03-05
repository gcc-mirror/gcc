/* PR sanitizer/88291 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address -Os" } */
/* { dg-additional-options "-mstringop-strategy=libcall" { target i?86-*-* x86_64-*-* } } */

void bar (void *, void *);

void
foo (void)
{
  int b;
  char __attribute__((aligned(16))) a[(1 << 20) + 1];
  bar (&a, &b);
}
