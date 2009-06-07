/* { dg-do compile } */
/* { dg-options "-Wunused-label" } */

extern void f2 ();

void
f1 ()
{
 l1: f2 ();				/* { dg-warning "not used" } */
 l2: __attribute__ ((unused)) f2 ();
 l3: ; f2 ();				/* { dg-warning "not used" } */
 l4: __attribute__ ((unused)) ; f2 ();
}
