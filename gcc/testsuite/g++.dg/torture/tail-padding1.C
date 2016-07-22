// Test that initializing a non-POD base with a trivial copy ctor doesn't
// clobber tail padding.

// { dg-do run }

struct X { ~X() {} int n; char d; };
struct Y { Y(); char c[3]; };
struct Z : X, virtual Y { Z(); };

X f() { X nrvo; __builtin_memset(&nrvo, 0, sizeof(X)); return nrvo; }
Z::Z() : Y(), X(f()) {}
Y::Y() { c[0] = 1; }

int main() {
  Z z;
  if (z.c[0] != 1)
    __builtin_abort ();
}
