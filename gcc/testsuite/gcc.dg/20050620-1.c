/* PR middle-end/22028 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  struct { int i[]; } u;	/* { dg-error "flexible array member" } */
}

void
bar (void)
{
  struct { struct a b; } c;	/* { dg-error "has incomplete type" } */
}
