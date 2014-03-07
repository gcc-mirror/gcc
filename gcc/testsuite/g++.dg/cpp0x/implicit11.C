// Test that we consider base dtors in determining whether
// a derived ctor is deleted even if the ctor is trivial.
// { dg-do compile { target c++11 } }

struct A
{
  ~A() = delete;		// { dg-message "declared here" }
};

struct B: A { };		// { dg-error "deleted" }

extern B eb;
int main()
{
  B* b1 = new B;		// { dg-error "use of deleted function" }
  B* b2 = new B(eb);		// { dg-error "use of deleted function" }
}
