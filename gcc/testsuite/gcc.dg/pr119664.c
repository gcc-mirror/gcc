/* PR target/119664 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

struct S { unsigned : 1, a : 1; } *s;
int foo (void);
void bar (void);

int
baz (void)
{
  int a = s->a;
  bar ();
  return a && foo ();
}
