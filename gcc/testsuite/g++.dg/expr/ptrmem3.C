struct S {};
struct T : public S {};

void f() {
  int S::*s;
  int T::*t;
  (t ? t : s) == t;
}

