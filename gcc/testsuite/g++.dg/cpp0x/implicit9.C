// Test that private base dtor makes derived ctor deleted
// { dg-do compile { target c++11 } }

struct A
{
  A();
private:
  ~A();				// { dg-message "private" }
};

struct B: A { };		// { dg-error "implicitly deleted|context" }
B * b = new B;			// { dg-error "deleted" }
