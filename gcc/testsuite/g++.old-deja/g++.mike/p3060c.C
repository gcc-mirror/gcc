// { dg-do assemble  }
// A new problem with my pointer to member function work.
// prms-id: 3060

class Foo
{
 public:
  int x;
  int y;
  Foo (int i, int j) { x = i; y = j; }
  operator int ();
};

int Foo::operator int() { return x; } // { dg-error "return" } can't specify return type

Foo foo(10, 11);

int
main()
{
  int Foo::* pmi = &Foo::y;
  return foo.*pmi;
}
