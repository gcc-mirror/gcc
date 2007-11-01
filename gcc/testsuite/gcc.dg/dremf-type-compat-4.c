/* Test for bogus diagnostics for dremf definition, as in bug 16666.
   The GNU extension permitting a prototype to override the promotion
   of old-style parameter declarations should only apply when the
   prototype is visible, not for a built-in prototype.  */
/* { dg-do compile } */
/* { dg-options "" } */

float
dremf(x, y) /* { dg-warning "conflicting types for built-in function 'dremf'" } */
     float x, y;
{
  return x + y;
}
