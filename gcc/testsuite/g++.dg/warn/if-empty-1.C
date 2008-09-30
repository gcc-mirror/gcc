/* Test diagnostics for empty bodies in if / else.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wempty-body" } */

void
f (int x)
{
  if (x)
    ; /* { dg-warning "suggest braces around empty body in an" } */
  if (x)
    ; /* By design we don't warn in this case.  */
  else
    (void)0;
  if (x)
    (void)0;
  else
    ; /* { dg-warning "suggest braces around empty body in an" } */
  if (x)
    (void)0;
  else
    (void)0;
}
