// { dg-do compile }

// Origin: David Baron <dbaron@fas.harvard.edu>

// PR c++/3765: Changing access from public to private by member
// using declaration.

class A
{
  public:
    int foo() { return 1; }	// { dg-error "inaccessible" }
};

class B : public A
{
  private:
    using A::foo;
};

int main()
{
  B b;
  return b.foo();		// { dg-error "this context" }
}
