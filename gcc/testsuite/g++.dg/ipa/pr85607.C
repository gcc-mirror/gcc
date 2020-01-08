// { dg-do compile }
/* { dg-options "-O2" } */

class A;	// { dg-message "7:forward declaration of 'class A'" }

A *a;		// { dg-warning "4:'a' has incomplete type" }

int
main (int argc, char **argv)
{
  delete a;	// { dg-warning "3:possible problem detected in invocation of .operator delete." "warn" }
  // { dg-message "3:neither the destructor nor the class-specific" "note" { target *-*-* } .-1 }
  return 0;
}
