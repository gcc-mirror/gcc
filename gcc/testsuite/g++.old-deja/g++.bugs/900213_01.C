// { dg-do assemble  }
// g++ 1.36.1 bug 900213_01

// g++ incorrectly diagnoses the error when an attempt is made to reference
// a non-static data-member without an object indication.

// Similar attempts to reference non-static function-members are correctly
// diagnosed by g++.

// Cfront 2.0 passes this test.

// keywords: non-static members, member pointers, scope resolution

struct struct0 {
  int struct0_data_member_0;		/* { dg-error "" } gets error from below */
  int struct0_function_member_0 ();
};

int i;

void global_function_0 ()
{
  i = struct0::struct0_data_member_0;		/* { dg-error "" } mishandled by g++ */
  //i = struct0::struct0_function_member_0 ();	/* gets caught by g++ */
}

int main () { return 0; }
