// { dg-do assemble  }
class Parent {
public:
  Parent() {}
  Parent( char *s ) {}
};

class Child : public Parent {		// { dg-error "" } called
};

int main() {
  Child c( "String initializer" );	// { dg-error "" } bad
  return 0;
}
