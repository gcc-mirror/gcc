/* Test diagnostics for addresses of labels and computed gotos.  Test
   with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

void
f (void)
{
  void *p = &&a; /* { dg-warning "warning: taking the address of a label is non-standard" } */
  goto *p; /* { dg-warning "warning: ISO C forbids 'goto \\*expr;'" } */
 a: ;
}
