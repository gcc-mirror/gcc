// g++ 1.36.1 bug 900205_04

// g++ allows a class for which an implicit default X::X() constructor must
// be created (implicitly by the compiler) to be derived from another class
// which does not have its own default X::X() constructor.  This is illegal.

// Cfront 2.0 passes this test.

// keywords: default constructor, inheritance

struct struct0 {
  int data_member;

  struct0 (int, void *);	// suppresses implicit default constructor
};

struct0::struct0 (int, void *)
{
}

struct struct0_derived_struct_0 : public struct0 { // ERROR - 
};

// struct0_derived_struct_0 object;	// would give g++ error if compiled

int main () { return 0; }
