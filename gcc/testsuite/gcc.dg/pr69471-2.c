/* { dg-do compile } */
/* { dg-options "-gstabs2 -gdwarf-4 -gstabs3" } */
/* { dg-error "conflicts with prior selectio" "" { target *-*-* } 0 } */

void
foo (void)
{
}
