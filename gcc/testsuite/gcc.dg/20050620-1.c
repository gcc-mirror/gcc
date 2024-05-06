/* PR middle-end/22028 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  struct { int i[]; } u;
}

void
bar (void)
{
  struct { struct a b; } c;	/* { dg-error "has incomplete type" } */
}
