/* PR c/18946 */
/* { dg-do compile } */
/* { dg-options "-Wshadow -std=gnu89" } */

void bar (void)
{
  {
    if (foo)		/* { dg-error "undeclared|for each" } */
      foo ();		/* { dg-warning "shadows previous" } */
  }
}

void baz (void)
{
  if (foo)		/* { dg-error "undeclared" } */
    {
      int foo;		/* { dg-warning "shadows previous" } */
    }
}
