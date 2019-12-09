// { dg-do assemble  }
class Parent {
public:
  Parent() {}
  Parent( char *s ) {}
};

class Child : public Parent {		// { dg-message "note" "" { target c++17_down } } called
};

int main() {
  Child c( "String initializer" );	// { dg-error "match" "" { target c++17_down } } bad
// { dg-error "forbids converting a string constant" "" { target c++2a } .-1 }
  return 0;
}
