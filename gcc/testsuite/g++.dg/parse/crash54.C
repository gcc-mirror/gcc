// PR c++/42057

struct A; // { dg-message "forward declaration" }

struct B
{
  virtual B* foo(A);
};

struct C : virtual B
{
  virtual C* foo(A) { return 0; } // { dg-error "incomplete type" }
};

C c;
