/* PR c/85318 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wc90-c99-compat -pedantic-errors" } */

extern void bar (int);

void
foo (int n)
{
  for (int i = 0; i < n; i++) /* { dg-warning "ISO C90 does not support .for. loop" } */
    bar (i);
}
