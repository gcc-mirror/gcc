/* PR c/85318 */
/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */

extern void bar (int);

void
foo (int n)
{
  for (int i = 0; i < n; i++) /* { dg-bogus "ISO C90 does not support .for. loop" } */
    bar (i);
}
