struct X {
  void f (int = 4, char = 'r');	// ERROR - 
  void g (int = 4, char = 'r');	// ERROR - 
};

void
X::f (int i = 4, char x = 'r')
{ }				// ERROR - duplicate default args

void
X::g (int i = 9, char x = 's')
{ }				// ERROR - duplicate default args
