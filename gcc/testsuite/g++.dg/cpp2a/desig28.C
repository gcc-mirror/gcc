// PR c++/110114
// { dg-do compile { target c++20 } }

struct A {
    int a,b;
};

struct B;

void foo(const A &) {}
void foo(const B &) {}

int
main ()
{
  foo({.a=0});
}
