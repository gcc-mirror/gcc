// Test the CWG 2273 inheritedness tiebreaker doesn't apply to
// non-constructors.

struct A
{
  void f(short,int=0);
  void g(char,int=0);
};

struct B:A
{
  using A::f;
  void f(short);
  using A::g;
  void g(short);
};

int main()
{
  B().f(1);			// { dg-error "ambiguous" }
  B().f(1,2);			// OK, base f can still be called with two args
  B().g(1);			// { dg-error "ambiguous" }
}
