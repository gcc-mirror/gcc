// { dg-do compile }

// Contributed by Brian Gaeke; public domain.

// 5 If the object being deleted has incomplete class type at the
// point of deletion and the complete class has a non-trivial
// destructor or a deallocation function, the behavior is undefined.

// (But the deletion does not constitute an ill-formed program. So the
// program should nevertheless compile, but it should give a warning.)

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
