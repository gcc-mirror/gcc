// A new problem with my pointer to member function work.
// Build don't link:
// prms-id: 3060

class Foo
{
 public:
  int x;
  int y;
  Foo (int i, int j) { x = i; y = j; }
  operator int ();
};

int Foo::operator int() { return x; } // WARNING - can't specify return type

Foo foo(10, 11);

int
main()
{
  int Foo::* pmi = &Foo::y;
  return foo.*pmi;
}
