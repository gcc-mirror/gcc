/* Test diagnostics for addresses of labels and computed gotos.  Test
   with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (void)
{
  void *p = &&a;
  goto *p;
 a: ;
}
