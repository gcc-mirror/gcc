/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void
g ()
{
  if (int i = 42);		/* { dg-error "ISO C does not support if declarations before C2Y" } */
  if (int i = 42; i > 10);	/* { dg-error "ISO C does not support if declarations before C2Y" } */
  if (int i, j; i = 42);	/* { dg-error "ISO C does not support if declarations before C2Y" } */
  switch (int i = 42);		/* { dg-error "ISO C does not support if declarations before C2Y" } */
  switch (int i = 42; i);	/* { dg-error "ISO C does not support if declarations before C2Y" } */
  switch (int i, j; i = 42);	/* { dg-error "ISO C does not support if declarations before C2Y" } */
}
