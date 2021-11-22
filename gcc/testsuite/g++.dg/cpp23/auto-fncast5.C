// PR c++/103049
// P0849R8 - auto(x)
// { dg-do compile { target c++23 } }

struct X {
  X() = default;
  X(const X&) = delete;
};

void
g ()
{
  X x;
  +X(x); // { dg-error "use of deleted function" }
  +auto(x); // { dg-error "use of deleted function" }
}

class A;
void f(A);

class A {
    int x;

public:
    A();

    auto run() {
        f(A(*this));
        f(auto(*this));
    }

protected:
    A(const A&);
};

void z () {
  A a;
  a.run ();
}
