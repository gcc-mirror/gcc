// Test for hiding of used base functions when all the conversion sequences are
// equivalent, needed to avoid a regression on inherited default ctors.

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
  B().f(1);			// OK, derived f hides base f for single arg
  B().f(1,2);			// OK, base f can still be called with two args
  B().g(1);			// { dg-error "" } signatures differ, ambiguous
}
