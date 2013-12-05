// Negative test for defaulted/deleted fns.
// { dg-options "-std=c++11" }

void f();			// { dg-message "previous" }
void f() = delete;		// { dg-error "deleted" }

struct A
{
  A() { }			// { dg-message "previous" }
  void f() = default;		// { dg-error "default" }
};

A::A() = default;		// { dg-error "redefinition" }

void g() {}			// { dg-message "previous" }
void g() = delete;		// { dg-error "redefinition" }

struct B // { dg-message "user-provided default constructor" }
{
  int i;
  B() = default;		// { dg-message "not user-provided" }
};

const B b;			// { dg-error "uninitialized const" }

struct C
{
  virtual void f() = delete;	// { dg-error "overriding deleted" }
};

struct D: public C
{
  virtual void f();		// { dg-error "non-deleted function" }
};

struct E
{
  const B b;
  E() { }			// { dg-error "uninitialized" }
};

struct F
{
  F() = default;
  F(const F&) = delete;		// { dg-message "declared" }
};

struct G
{
  G();
};

// ctor defaulted after class defn is not trivial
G::G() = default;

union U
{
  G g;				// { dg-error "union member.*non-trivial" }
};

int main()
{
  F f;
  F f2(f);			// { dg-error "use" }
  const B* b = new const B;		// { dg-error "uninitialized const" }
  U u;				// { dg-error "deleted" }
}

// { dg-prune-output "implicitly deleted because" }
