// { dg-do assemble  }
// g++ 1.37.1 bug 900511_03

// g++ does not properly shadow names of types with names of data members
// in cases where the type names in question are used in the context of
// formal parameters lists for member functions.

// keywords: typedef names, shadowing, scope, formal parameter list

class class0;

struct struct1 {
  int class0;
  void member_function (class0 *); // { dg-error "" } invalid use of struct-local member
};

void class1::member_function (class0 *p) {	// { dg-error "" } 
}

int main () { return 0; }
