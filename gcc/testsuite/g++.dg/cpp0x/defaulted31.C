// PR c++/39164
// { dg-do compile { target c++11 } }

struct A
{
  A() { }			// { dg-message "defined" }
  ~A() = default;		// { dg-message "defaulted" }
};

A::A() = default;		// { dg-error "redefinition" }
A::~A() noexcept (true) { }	// { dg-error "defaulted" }

int main()
{
  A a;
}
