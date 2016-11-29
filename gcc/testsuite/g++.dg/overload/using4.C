struct A
{
  void f();
};

struct B
{
  void f();
};

struct C: A,B {
  using A::f;
  using B::f;
};

int main()
{
  C().f();			// { dg-error "ambiguous" }
}
