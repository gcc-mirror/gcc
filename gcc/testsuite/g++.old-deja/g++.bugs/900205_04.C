// { dg-do assemble  }
// g++ 1.36.1 bug 900205_04

// g++ allows a class for which an implicit default X::X() constructor must
// be created (implicitly by the compiler) to be derived from another class
// which does not have its own default X::X() constructor.  This is illegal.

// Cfront 2.0 passes this test.

// keywords: default constructor, inheritance

// In ISO C++ 1998, such a derived class is not ill-formed, but if the
// implicitly-declared constructor is used, then it is implicitly
// defined and found to be ill-formed.

struct struct0 { // { dg-error "note" }
  int data_member;

  struct0 (int, void *);	// suppresses implicit default constructor
};

struct0::struct0 (int, void *) // { dg-error "note" }
{
}

struct struct0_derived_struct_0 : public struct0 { // { dg-error "no matching" }
};

struct0_derived_struct_0 object; // { dg-error "synthesized" }

int main () { return 0; }
