// { dg-do assemble  }
// g++ 1.37.1 bug 900519_07

// It is illegal to specify or to use array-of-reference types, yet g++
// allows them to be specified (in typedef statements and in declarations)
// and to be used (in declarations).

// keywords: reference types, array types

int i;
int j;

typedef int& int_ref;
typedef int_ref int_ref_array_type[2];		// { dg-error "" } missed

int& int_ref_array_obj0[2] = { i, j };		// { dg-error "" } missed
int_ref int_ref_array_obj1[2] = { i, j };	// { dg-error "" } missed
