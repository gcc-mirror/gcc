// { dg-do assemble  }
// { dg-options "-pedantic-errors" }

class Base {
public:
  int foo;
};

class Derived : public Base {
public:
  int bar;
};

void func(Base&);			// { dg-error "passing argument 1" } 

void func2(const Derived& d) {
  func(d);				// { dg-error "invalid initialization" }
}

void
foo (int& a)				// { dg-error "in passing argument 1" } 
{
}

int main ()
{
  int b;
  const int*const a = &b;
  *a = 10;				// { dg-error "read-only location" }
  foo (*a);				// { dg-error "invalid initialization" }
  return 0;
}
