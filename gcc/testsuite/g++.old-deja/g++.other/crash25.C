// { dg-do assemble  }
// { dg-options "-fshow-column" }
// Origin: Jakub Jelinek <jakub@redhat.com>

class X { // { dg-error "1:new types may not be defined in a return type" "new types" }
// { dg-message "1:\\(perhaps a semicolon is missing after the definition of 'X'\\)" "note" { target *-*-* } .-1 }
public:
  X();
  virtual ~X();
}

X::x()	// { dg-error "6:no 'X X::x\\(\\)' member function declared in class 'X'" }
{
}

X::~x()	// { dg-error "6:expected class-name before '\\(' token" }
{				
}
