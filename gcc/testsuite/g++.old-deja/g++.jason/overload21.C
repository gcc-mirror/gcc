// { dg-do assemble  }
struct X {
  void f (int = 4, char = 'r');	// { dg-error "" } 
  void g (int = 4, char = 'r');	// { dg-error "" } 
};

void
X::f (int i = 4, char x = 'r')
{ }				// { dg-error "" } duplicate default args

void
X::g (int i = 9, char x = 's')
{ }				// { dg-error "" } duplicate default args
