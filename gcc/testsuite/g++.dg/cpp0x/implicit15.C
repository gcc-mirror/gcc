// PR c++/88122
// { dg-do compile { target c++11 } }

struct A {
  A (...);	// { dg-message "candidate" }
  A ();		// { dg-message "candidate" }
};
struct B : A {
  using A::A;	// { dg-error "is ambiguous" }
		// { dg-message "is implicitly deleted because the default definition would be ill-formed" "" { target *-*-* } .-1 }
} b{3};		// { dg-error "use of deleted function" }
