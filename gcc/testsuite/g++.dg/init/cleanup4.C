// PR c++/79679
// { dg-do run }

int count;
struct S {
    S() { ++count; }
    S(const S&) { ++count; }
    ~S() { --count; }
};

struct T {
    T(S) {}
};

int main() {
  {
    S s;
    T u(s);
  }
  if (count)
    __builtin_abort();
}
