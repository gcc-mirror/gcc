/* Test diagnostics for empty bodies in if / else.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wextra" } */

void
f (int x)
{
  if (x)
    ; /* { dg-warning "warning: empty body in an if-statement" } */
  if (x)
    ; /* By design we don't warn in this case.  */
  else
    (void)0;
  if (x)
    (void)0;
  else
    ; /* { dg-warning "warning: empty body in an else-statement" } */
  if (x)
    (void)0;
  else
    (void)0;
}
