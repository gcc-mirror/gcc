/* PR sanitizer/81223 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

void bar ();

void
foo (int x)
{
  struct S { char a[x]; } v;
  bar (v);
}
