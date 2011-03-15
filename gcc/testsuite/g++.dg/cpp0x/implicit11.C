// Test that we consider base dtors in determining whether
// a derived ctor is deleted even if the ctor is trivial.
// { dg-options -std=c++0x }

struct A
{
  ~A() = delete;		// { dg-error "declared here" }
};

struct B: A { };		// { dg-error "deleted" }

extern B eb;
int main()
{
  B* b1 = new B;		// { dg-error "use of deleted function" "" { xfail *-*-* } }
  B* b2 = new B(eb);		// { dg-error "use of deleted function" }
}
