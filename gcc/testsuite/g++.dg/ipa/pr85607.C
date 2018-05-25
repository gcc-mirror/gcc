// { dg-do compile }
/* { dg-options "-O2" } */

class A;	// { dg-message "forward declaration of 'class A'" }

A *a;		// { dg-warning "'a' has incomplete type" }

int
main (int argc, char **argv)
{
  delete a;	// { dg-warning "delete" "warn" }
  // { dg-message "note" "note" { target *-*-* } .-1 }
  return 0;
}
