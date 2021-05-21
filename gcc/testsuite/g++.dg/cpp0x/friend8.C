// PR c++/100596
// { dg-do compile { target c++11 } }

struct A
{
  __attribute((deprecated)) friend void f(A); // part of A API, definition in .C
  [[deprecated]] friend void f2(A); // { dg-warning "ignored" }
};

int main()
{
  A a;
  f(a); // { dg-warning "is deprecated" }
  f2(a);
}
