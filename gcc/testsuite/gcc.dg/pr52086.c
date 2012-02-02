/* PR target/52086 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpic" { target fpic } } */

struct S { char a; char b[100]; };
int bar (void);
int baz (int);

void
foo (struct S *x)
{
  if (bar () & 1)
    {
      char c = bar ();
      baz (4);
      x->a += c;
      while (x->a)
	x->b[c] = bar ();
    }
}
