// { dg-do compile { target c++11 } }
struct A {
  A () = delete;	// { dg-message "declared here" "" { target c++2a } }
};
struct B {
  B () = default;
  int b = 0;
};
struct C {
  C (C&&) = default;	// { dg-message "candidate" "" { target c++2a } }
  int c, d;
};
A a {};			// { dg-error "use of deleted function" "" { target c++2a } }
B b = {1};		// { dg-error "could not convert" "" { target { c++11_only || c++2a } } }
C *c = new C {2, 3};	// { dg-error "no matching function for call to" "" { target c++2a } }
