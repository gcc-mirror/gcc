// Build don't link:
// Special g++ Options: -pedantic-errors

class Base {
public:
  int foo;
};

class Derived : public Base {
public:
  int bar;
};

void func(Base&);		// ERROR - 

void func2(const Derived& d) {
  func(d);			// ERROR - this is bad
}

void
foo (int& a)
{				// ERROR - 
}

int main ()
{
  int b;
  const int*const a = &b;
  *a = 10;				// ERROR - it's const
  foo (*a);				// ERROR - it's const
  return 0;
}
