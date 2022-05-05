// PR c++/96876
// { dg-do run { target c++11 } }

int d;
struct B {
  ~B() { ++d; }
};

struct C1 { B b; int n; };
struct C2 { int i; B b; int n; };

int f() { throw 24; return 42; }
int dummy;
int g() { ++dummy; return 42; }

int main() {
  try {
    C1 c{{}, f()};
  } catch (...) { }

  try {
    C2 c{g(), {}, f()};
  } catch (...) { }

  if (d != 2)
    __builtin_abort ();
}
