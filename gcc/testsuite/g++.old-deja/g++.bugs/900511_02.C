// { dg-do assemble  }
// g++ 1.37.1 bug 900511_02

// g++ does not properly shadow names of types with names of data members
// in cases where the type names in question are used in the context of
// formal parameters lists for member functions.

// keywords: typedef names, shadowing, scope, formal parameter list

// cfront 2.0 passes this test.

enum enum0 { enum0_value_0 };	

struct struct0 {
  int enum0;			
  void member_function (enum0 e); // { dg-error "" } invalid use of struct-local member
};

void class0::member_function (enum0 e) {	// { dg-error "" } syntax error
}

int main () { return 0; }
