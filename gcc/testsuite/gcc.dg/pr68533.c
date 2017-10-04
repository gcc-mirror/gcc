/* PR c/68533 */
/* { dg-do compile } */
/* { dg-options "" } */

struct T { int t; };

void
f1 (
  struct S *	/* { dg-warning "declared inside parameter list will not be visible outside of this definition or declaration" } */
  x,
  struct T *
  y
   )
{
  y->t = 4;
}

void
f2 (
  struct {int s;} * /* { dg-warning "anonymous struct declared inside parameter list will not be visible outside of this definition or declaration" } */
  x,
  struct T *
  y
   )
{
  y->t = 5;
}

void
f3 (
  const void	/* { dg-error "'void' as only parameter may not be qualified" } */
   )
{
}

void
f4 (
   void,	/* { dg-error "'void' must be the only parameter" } */
   ...
   )
{
}

void
f5 (
   int
   x;		/* { dg-error "parameter 'x' has just a forward declaration" } */
   int y
   )
{
}

void
f6 (
   int
   x,
   void	/* { dg-error "'void' must be the only parameter" } */
   )
{
}

void
f7 (
   void,	/* { dg-error "'void' must be the only parameter" } */
   int y
   )
{
}
