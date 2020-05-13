// { dg-do compile { target c++20 } }

[[no_unique_address]] struct B { }; // { dg-warning "attribute" }
[[no_unique_address]] int i;	    // { dg-warning "attribute" }
[[no_unique_address]] void f();	    // { dg-warning "attribute" }

struct A
{
  [[no_unique_address]] B b;
  [[no_unique_address]] void f();   // { dg-warning "attribute" }
  [[no_unique_address]] static B c; // { dg-warning "attribute" }
  int i;
};
