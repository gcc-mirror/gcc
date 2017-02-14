// PR c++/71835
// { dg-do compile }

typedef void T (int);
struct A { operator T * (); };	// { dg-message "candidate" }
struct B { operator T * (); };	// { dg-message "candidate" }
struct C : A, B {} c;

void
foo ()
{
  c (0);		// { dg-error "is ambiguous" }
}
