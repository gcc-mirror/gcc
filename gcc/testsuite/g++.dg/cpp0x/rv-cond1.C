// PR c++/58714
// { dg-do compile { target c++11 } }

struct X {
  X& operator=(const X&) = delete;
  X& operator=(X&& ) = default;
};

void f(bool t) {
  X a, b;
  *(t ? &a : &b) = X();
  (t ? a : b) = X();
}
