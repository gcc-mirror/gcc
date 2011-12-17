// PR c++/51406
// { dg-do run }
// { dg-options "-std=c++0x" }

extern "C" int printf(const char *,...);
extern "C" void abort();

struct A { int a; A() : a(1) {} };
struct B { int b; B() : b(2) {} };
struct X : A, B {};

int main() {
    X x;
    int a=static_cast<A&&>(x).a;
    int b=static_cast<B&&>(x).b;
    // printf ("%d %d\n", a, b);
    if (a!=1 || b!=2) abort();
}
