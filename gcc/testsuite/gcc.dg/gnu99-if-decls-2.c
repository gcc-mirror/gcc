/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
g ()
{
  if (int i = 42);
  if (int i = 42; i > 10);
  if (int i, j; i = 42);
  switch (int i = 42);
  switch (int i = 42; i);
  switch (int i, j; i = 42);
}
