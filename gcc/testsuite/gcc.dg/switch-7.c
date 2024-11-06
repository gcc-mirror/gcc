/* Test diagnostics for switch statements and labels therein.  Test
   for case ranges with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors -std=c23" } */

void
f (int a)
{
  switch (a)
    {
    case 0 ... 0: ; /* { dg-error "ISO C does not support range expressions in switch statements before C2Y" } */
    }
}
