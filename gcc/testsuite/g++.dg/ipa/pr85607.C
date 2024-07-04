// { dg-do compile }
/* { dg-options "-O2" } */

class A;	// { dg-message "7:forward declaration of 'class A'" }

A *a;		// { dg-warning "4:'a' has incomplete type" "" { target c++23_down } }

int
main (int argc, char **argv)
{
  delete a;	// { dg-warning "3:possible problem detected in invocation of .operator delete." "warn" { target c++23_down } }
  // { dg-message "3:neither the destructor nor the class-specific" "note" { target c++23_down } .-1 }
  // { dg-error "operator 'delete' used on incomplete type" "" { target c++26 } .-2 }
  return 0;
}
