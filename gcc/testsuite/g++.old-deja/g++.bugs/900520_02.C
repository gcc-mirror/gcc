// g++ 1.37.1 bug 900520_02

// g++ fails to allow a reference to an unbounded array type to be passed
// into a formal parameter whose type is pointer-to-bounded-array type.

// Cases other than parameter passing in which similar initializations
// take place are allowed however.

// cfront 2.0 passes this test.

// keywords: reference types, initialization, parameter passing

typedef int b_array[3];
typedef int u_array[];

typedef b_array &b_array_ref;
typedef u_array &u_array_ref;

void take_b_array_ref (b_array_ref arg) { }

extern u_array u_array_gbl_obj;

u_array_ref u_array_ref_gbl_obj0 = u_array_gbl_obj;

b_array_ref b_array_ref_gbl_obj0 = u_array_ref_gbl_obj0;	// OK

void test_passing ()
{
  take_b_array_ref (u_array_ref_gbl_obj0);	// gets bogus error
}

b_array u_array_gbl_obj;

int main () { return 0; }
