// prms-id: 2394

class Foo {
 public:
  int x;
  int y;
  Foo(int i, int j) { x = i; y = j; }
} foo(10, 11);

class Wasted { int unsed; };

class Bar : Wasted, public Foo {
public:
  Bar() : Foo(12, 13) { }
} bar;

int
test0() {
  int Foo::* pmi = &Foo::y;
  return (int)(foo.*pmi);
}

int
test1() {
  int Foo::* pmi = &Foo::y;
  return (int)(bar.*pmi);
}

int
main() {
  if (test0() != 11)
    return 1;
  if (test1() != 13)
    return 2;
  return 0;
}
