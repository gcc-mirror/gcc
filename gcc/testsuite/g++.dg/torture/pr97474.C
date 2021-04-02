// PR c++/97474
// { dg-do run }

extern "C" int printf (const char *, ...);
extern "C" void abort ();

struct A {
    int a;
    int& b;

    A(int x) : a(x), b(a) {}
    A(const A& other) : a(other.a), b(a) {}
    A() : a(0), b(a) {}
};

int foo(A a) {
    a.a *= a.b;
    return a.b;
}


int main() {
    A a(3);

    int r = foo (a);
    if (r != 9)
      abort ();
}
