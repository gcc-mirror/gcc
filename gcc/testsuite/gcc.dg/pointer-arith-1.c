/* Test diagnostics for arithmetic on void and function pointers.
   Test with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void *p;
void (*f)(void);

void
g (void)
{
  p + 0;
  p + 1;
  0 + p;
  1 + p;
  p - 0;
  p - 1;
  p += 0;
  p += 1;
  p -= 0;
  p -= 1;
  f + 0;
  f + 1;
  0 + f;
  1 + f;
  f - 0;
  f - 1;
  f += 0;
  f += 1;
  f -= 0;
  f -= 1;
  p[0]; /* { dg-warning "warning: dereferencing 'void \\*' pointer" } */
  0[p]; /* { dg-warning "warning: dereferencing 'void \\*' pointer" } */
  f[0]; /* { dg-error "error: subscripted value is pointer to function" } */
  0[f]; /* { dg-error "error: subscripted value is pointer to function" } */
  p - p;
  f - f;
}
