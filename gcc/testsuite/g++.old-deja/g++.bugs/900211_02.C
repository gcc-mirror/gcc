// g++ 1.36.1 bug 900211_02

// g++ allows you to explicitly specify the return type for a type conversion
// operator.

// The Cfront 2.0 Reference Manual (12.3.2) says that this in not allowed.

// Cfront 2.0 passes this test.

// keywords: type conversion operators, return type

struct struct0 { int member_0; };

struct0 struct0_object_0;

struct struct1 {
  struct0 operator struct0 ();		/* ERROR - */
};

struct0 struct1::operator struct0 () {	// ERROR - 
  return struct0_object_0;
}

int main () { return 0; }
