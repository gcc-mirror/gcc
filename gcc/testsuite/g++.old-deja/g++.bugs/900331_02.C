// g++ 1.37.1 bug 900331_02

// g++ fails to treat conditional expressions which yield composite type
// (i.e. struct type, union type, or class type) lvalues as if they did
// in fact yield lvalues in all cases.

// Cfront 2.0 passes this test.

// keywords: conditional operator?:, lvalues, composite types

struct struct0 {
  int data_member;
};

struct0 object0;
struct0 object1;
struct0 object2;

int i;

void function0 ()
{
  (i ? object0 : object1).data_member = 99;	// gets bogus error
  (i ? object0 : object1) = object2;		// gets bogus error
}

int main () { return 0; }
