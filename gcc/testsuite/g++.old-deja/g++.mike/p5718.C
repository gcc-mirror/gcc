// prms-id: 5718

class Base {
  int i;
public:
  Base() { i = 42; };
};


class Mixin {
  int j;
public:
  Mixin() { j = 42; }
};


class Derived : public Base, public Mixin {
public:
  Derived() { };
  Derived & operator=(Mixin & m) { return *this; };
};


void
testFunct(Derived * arg) {
  Mixin temp;

  (Mixin &)(*arg) = temp;		// gets bogus error 
}


int
main(int argc, char *argv[]) {
  Derived temp;

  testFunct(&temp);
}
