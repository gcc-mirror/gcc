// g++ 1.37.1 bug 900331_03

// Special Options: -ansi

// The following code causes g++ to abort.

// Curiously, the abort does not occur if the -pedantic option is used.

// Cfront 2.0 passes this test.

// Keywords: abort, conditional operator?:, lvalues, composite types

struct struct0 {
  int data_member;

  virtual void function_member () {}	// contributes to the abort
};

struct0 object0;
struct0 object1;
struct0 object2;

int i;

void function0 ()
{
  object2 = (i ? object0 : object1);		// OK
  (i ? object0 : object1) = object2;		// causes abort
}

int main () { return 0; }
