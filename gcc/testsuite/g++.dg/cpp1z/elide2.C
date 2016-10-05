// DR 1697
// { dg-do run { target c++11 } }

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

int i;
struct S {
  ~S() { assert (i++ == 2); }
};
struct X {
  X() { assert (i++ == 0); }
  X(const X&);
};
struct T {
  S &&s;
  X x;
};
void f() { assert (i++ == 1); }
int main() {
  {
    T t = T{ {}, {} };
    f();
  }
  assert (i == 3);
}
