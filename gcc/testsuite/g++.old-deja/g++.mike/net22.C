// { dg-do assemble  }
class Parent {
public:
  Parent() {}
  Parent( char *s ) {}
};

class Child : public Parent {		// { dg-message "note" } called
};

int main() {
  Child c( "String initializer" );	// { dg-error "match" } bad
  return 0;
}
