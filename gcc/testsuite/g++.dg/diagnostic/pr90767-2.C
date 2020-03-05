// PR c++/90767
// { dg-do compile }

struct A {
  struct B { B (int) {} };

  template <typename T>
  void foo ()
  {
    int x = 0;
    bar (x);	// { dg-error "cannot convert 'int' to 'A::B&'" }
  }

  void bar (B &arg) {}	// { dg-message "initializing argument 1" }
};
