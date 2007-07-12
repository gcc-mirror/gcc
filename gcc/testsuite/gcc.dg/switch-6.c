/* Test diagnostics for switch statements and labels therein.  Test
   for case ranges with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

void
f (int a)
{
  switch (a)
    {
    case 0 ... 0: ; /* { dg-warning "range expressions in switch statements are non-standard" } */
    }
}
