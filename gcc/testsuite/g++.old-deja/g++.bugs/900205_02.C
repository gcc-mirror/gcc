// { dg-do assemble  }
// g++ 1.36.1 bug 900205_02

// g++ allows constructors to be defined which do not include
// initializations for reference members of their associated classes.

// Cfront 2.0 does not allow this.

// keywords: reference members, constructors, member initialization

int i;

class c0 {
  int &int_ref;
public:
  c0 () /* : int_ref(i) */ {	// { dg-error "" } reference needs initializer
  }
};

class c1 {
  int &int_ref;
public:
  c1 ();
};

c1::c1() /* : int_ref(i) */ {	// { dg-error "" } reference needs initializer
}

int main () { return 0; }
