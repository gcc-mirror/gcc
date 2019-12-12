// PR c++/89214
// { dg-do compile { target c++17 } }

struct A
{
  A (int);
};

struct BB
{
  A a;
};

struct B : BB
{
};

void
foo ()
{
  B b1 = {42};
  B b2 = {{42}};
  B b3 = {{{42}}};

  B b4 = B{42};
  B b5 = B{{42}};
  B b6 = B{{{42}}};

  B b7 = {B{42}};
  B b8 = {B{{42}}};
  B b9 = {B{{{42}}}};

  B b10 = {{B{42}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }
  B b11 = {{B{{42}}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }
  B b12 = {{B{{{42}}}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }

  B bb1{42};
  B bb2{{42}};
  B bb3{{{42}}};

  B bb7{B{42}};
  B bb8{B{{42}}};
  B bb9{B{{{42}}}};

  B bb10{{B{42}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }
  B bb11{{B{{42}}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }
  B bb12{{B{{{42}}}}}; // { dg-warning "initializing a base class of type .BB. results in object slicing" }
}
