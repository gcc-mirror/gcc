// { dg-do run }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR185: "Named" temporaries and copy elision 

extern "C" void abort(void);

struct A {
  mutable int value;
  explicit A(int i) : value(i) {}
  void mutate(int i) const { value = i; }
};

int foo() {
  A const& t = A(1);
  A n(t);          // can this copy be elided? NO!
  t.mutate(2);
  return n.value;  // can this return 2? NO!
}

int main()
{
  int x = foo();
  if (x != 1)
    abort();
  return 0;
}
