// Negative test for C++11 unrestricted unions
// { dg-do compile { target c++11 } }
// { dg-prune-output "implicitly deleted because" }

struct A
{
  A();
  A(const A&);
  ~A();
};

union B
{
  A a;				// { dg-error "union member" }
};

B b;				// { dg-error "B::B\\(\\)" "B::B" }
B b2(b);			// { dg-error "B::B\\(const B&\\)" "B::B" }

struct C
{
  union
  {
    A a;			// { dg-error "union member" }
  };
};

C c;				// { dg-error "C::C\\(\\)" "C::C" }
C c2(c);			// { dg-error "C::C\\(const C&\\)" "C::C" }

// { dg-error "B::~B" "B::~B" { target *-*-* } 17 }
// { dg-error "B::~B" "B::~B" { target *-*-* } 18 }
// { dg-error "C::~C" "C::~C" { target *-*-* } 28 }
// { dg-error "C::~C" "C::~C" { target *-*-* } 29 }
