// { dg-do assemble  }
struct X {
  void f (int = 4, char = 'r');	// { dg-error "previous specification" } 
  void g (int = 4, char = 'r');	// { dg-error "previous specification" } 
};

void
X::f (int i = 4, char x = 'r') // { dg-error "default argument" }
{ }

void
X::g (int i = 9, char x = 's') // { dg-error "default argument" }
{ }
