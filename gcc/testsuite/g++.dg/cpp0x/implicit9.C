// Test that private base dtor makes derived ctor deleted
// { dg-options -std=c++0x }

struct A
{
  A();
private:
  ~A();				// { dg-error "private" }
};

struct B: A { };		// { dg-error "implicitly deleted|context" }
B * b = new B;			// { dg-error "deleted" }
