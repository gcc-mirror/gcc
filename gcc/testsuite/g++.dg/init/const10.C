// PR C++/52369
// { dg-do compile { target c++11 } }

class B // { dg-message "implicitly deleted" }
        // { dg-error "uninitialized" "" { target c++11 } .-1 }
{
  int const v_; // { dg-message "should be initialized" }
};

struct D : B {}; // { dg-error "deleted" }

class A // { dg-message "implicitly deleted" }
	// { dg-error "uninitialized" "" { target c++11 } .-1 }
{
  int& ref; // { dg-message "should be initialized" }
};

struct C : A {}; // { dg-error "deleted" }

void f()
{
  D d; // { dg-error "use of deleted" }
  new D; // { dg-error "use of deleted" }
  D(); // { dg-error "use of deleted" }
  new D(); // { dg-error "use of deleted" }

  C c; // { dg-error "use of deleted" }
  new C; // { dg-error "use of deleted" }
  C(); // { dg-error "use of deleted" }
  new C(); // { dg-error "use of deleted" }
}


