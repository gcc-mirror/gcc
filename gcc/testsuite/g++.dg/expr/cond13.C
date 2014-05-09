// PR c++/54348

struct A {} a;
struct B {} b;

void f()
{
    false ? a : b;		// { dg-error "different types" }
}
