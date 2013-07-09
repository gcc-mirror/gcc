// PR c++/57437
// { dg-require-effective-target c++11 }

struct A {
  int i;

  A(): i(42) {}
  A(const A&) = default;
  A(A&& a): i(a.i) { a.i = 0; }
};

int main()
{
  A x;

  auto y = [x] () mutable {
    x.i++;
    return x;
  };

  if (y().i != 43)
    __builtin_abort ();

  if (y().i != 44)
    __builtin_abort ();
}
