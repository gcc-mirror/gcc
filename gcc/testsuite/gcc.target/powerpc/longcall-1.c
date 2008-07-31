/* PR target/35100 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-fpic" } */

void foo (void) __attribute__((__longcall__));
int baz (void) __attribute__((__longcall__));

int
bar (void)
{
  foo ();
  return baz () + 1;
}
