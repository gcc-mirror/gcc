/* Test diagnostics for addresses of labels and computed gotos.  Test
   with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

void
f (void)
{
  void *p = &&a; /* { dg-error "taking the address of a label is non-standard" } */
  goto *p; /* { dg-error "ISO C forbids 'goto \\*expr;'" } */
 a: ;
}
