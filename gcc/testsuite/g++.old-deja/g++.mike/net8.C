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

void func(Base&);		// { dg-error "" } 

void func2(const Derived& d) {
  func(d);			// { dg-error "" } this is bad
}

void
foo (int& a)
{				// { dg-error "" } 
}

int main ()
{
  int b;
  const int*const a = &b;
  *a = 10;				// { dg-error "" } it's const
  foo (*a);				// { dg-error "" } it's const
  return 0;
}
