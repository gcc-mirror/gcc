// { dg-do compile }

// Contributed by Brian Gaeke; public domain.

// 5 If the object being deleted has incomplete class type at the
// point of deletion and the complete class has a non-trivial
// destructor or a deallocation function, the behavior is undefined.

// (But the deletion does not constitute an ill-formed program. So the
// program should nevertheless compile, but it should give a warning.)

class A;	// { dg-warning "forward declaration of 'struct A'" "" }

A *a;		// { dg-warning "'a' has incomplete type" "" }

int
main (int argc, char **argv)
{
  delete a;	// { dg-warning "delete" "warn" }
  // { dg-message "note" "note" { target *-*-* } 19 }
  return 0;
}
