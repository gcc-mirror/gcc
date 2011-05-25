// { dg-do compile }
// { dg-options -fpermissive }

struct A
{
  int const i; // { dg-message "should be initialized" }
};

struct B
{
  int& r; // { dg-message "should be initialized" }
};

struct C
{
  int const i : 1; // { dg-message "should be initialized" }
};

void f()
{
  new A;  // { dg-warning "uninitialized" }
  new B;  // { dg-warning "uninitialized" }
  new C;  // { dg-warning "uninitialized" }
  C c;    // { dg-warning "uninitialized" }
  A a[1]; // { dg-warning "uninitialized" }
}
