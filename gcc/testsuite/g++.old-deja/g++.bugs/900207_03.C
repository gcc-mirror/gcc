// { dg-do run  }
// g++ 1.36.1 bug 900207_03

// g++ fails to allow objects of class or struct types to be initialized
// using "constructor syntax" in cases where an implicitly generated copy
// constructor would be invoked for the initialization, and where there is
// no explicitly specified constructor associated with the type of the
// object being initialized.

// Note that the type of the error changes depending upon whether or not the
// type being initialized has any virtual functions associated with it.

// Cfront 2.0 passes this test.

// keywords: implicit copy constructor, initialization


// Check construction for a type without virtual function members.

struct struct0 {
  int data_member;
};

struct0 struct0_gbl_object0;
struct0 struct0_gbl_object1 (struct0_gbl_object0);	// { dg-bogus "" } 

void struct0_test ()
{
  struct0 struct0_lcl_object1 (struct0_gbl_object0);	// { dg-bogus "" } 
}

// Check construction for a type with virtual function members.

struct struct1 {
  int data_member;

  virtual void function_member ();
};

void struct1::function_member () { }

struct1 struct1_gbl_object0;
struct1 struct1_gbl_object1 (struct1_gbl_object0);	// { dg-bogus "" } 

void struct1_test ()
{
  struct1 struct1_lcl_object1 (struct1_gbl_object0);	// { dg-bogus "" } 
}

int main () { return 0; }
