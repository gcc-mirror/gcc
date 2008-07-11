// { dg-do assemble  }
// { dg-options "-fshow-column" }
// Origin: Jakub Jelinek <jakub@redhat.com>

class X { // { dg-error "1: error: new types may not be defined in a return type|1: note: \\(perhaps a semicolon is missing after the definition of 'X'\\)" }
public:
  X();
  virtual ~X();
}

X::x()	// { dg-error "6: error: no 'X X::x\\(\\)' member function declared in class 'X'" }
{
}

X::~x()	// { dg-error "6: error: expected class-name before '\\(' token" }
{				
}
