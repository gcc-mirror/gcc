/* Test diagnostics for empty bodies in do / while.  */
/* { dg-do compile } */
/* { dg-options "-Wempty-body" } */

void
f (int x)
{
  do
    ; /* { dg-warning "empty body in" } */
  while (x--);

  do
  {} /* { dg-bogus "empty body in" } */
  while (++x < 10);
}
