// PR c++/113640
// { dg-do run { target c++23 } }

static int total;

struct A {
  A f(this auto self, int n) {
    total += n;
    return self;
  }
};

int main() {
  A a;
  a.f(1).f(42).f(100);
  if (total != 143)
    __builtin_abort();

  auto l = [](this auto self, int n) {
    total += n;
    return self;
  };
  total = 0;
  l(1)(42)(100);
  if (total != 143)
    __builtin_abort();
}
