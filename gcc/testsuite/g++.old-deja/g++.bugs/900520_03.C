// { dg-do assemble  }
// g++ 1.37.1 bug 900520_03

// The C++ Reference Manual says (in section 8.2.4):

//	When an identifier of array type appears in an expression, except
//	as the operand of sizeof or & or used to initialize a reference,
//	it is converted into a pointer to the first member of the array.

// One must assume from the verbage, that when the name of a non-const array
// object appears in one of the exempted contexts mentioned in this passage,
// that it is *not* automatically converted into a pointer value, but rather
// that it remains as an array type value, and that it may therefore also
// still be an lvalue, and may be used to initialize references.

// As the following code demonstrates, g++ does in fact treat the names
// of non-const array objects as valid initializers for reference-to-array
// type object in some (but not all) contexts.

// The exception is that g++ does not allow names which designate objects
// on incomplete array types to be used as actual parameters in function
// calls where the corresponding formal parameter is of a reference-to-array
// type.

// g++ does however allow other similar sorts of initializations of non-formal
// reference objects.

// 5/16/94 (jason): The 1/25/94 WP explicitly states in section 8.3.5 that
// parameter types may not contain pointers or references to arrays of unknown
// bound.  g++ is correct.

// keywords: reference types, array types, initialization, parameter passing

typedef int u_array[];
typedef u_array &u_array_ref;

void take_u_array_ref (u_array_ref arg) { } // { dg-error "" } reference to array of unknown bound in parmtype

extern u_array u_array_gbl_obj;
u_array_ref u_array_ref_gbl_obj0 = u_array_gbl_obj;	// OK

void test_local_initialization ()
{
  u_array_ref u_array_ref_lcl_obj0 = u_array_gbl_obj;	// OK
}

void test_passing ()
{
  take_u_array_ref (u_array_gbl_obj);
}
