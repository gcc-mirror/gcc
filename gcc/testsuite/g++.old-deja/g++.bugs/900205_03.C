// { dg-do assemble  }
// { dg-options "-Wreturn-type -pedantic-errors" }
// g++ 1.36.1 bug 900205_03

// Section 6.6.3 of the cfront 2.0 Reference Manual says "A return statement
// without an expression can be used only in functions that do not return
// a value, that is, a function with the return value type void..."

// Also in 6.6.3: "Flowing off the end of a function is equivalent to a
// return with no value; this is illegal in a value returning function."

// In contrast to the manual, g++ does not generate ERRORs for cases of
// "flowing off the end" of non-void functions.

// keywords: return statements, return type, void return, implicit return


struct struct00 { };

int global_function_0 () {
}					// { dg-error "" } 

struct00 global_function_1 () {
}					// { dg-error "" } 

struct struct0 {

  int struct0_member_function_0 () {
  }					// { dg-error "" } 

  struct0 struct0_member_function_1 () {
  }					// { dg-error "" } 
};

struct struct1 {

  int struct1_member_function_0 ();

  struct1 struct1_member_function_1 ();

};

int struct1_member_function_0 () {
}					// { dg-error "" } 

struct1 struct1::struct1_member_function_1 () {
}				        // { dg-error "" } 

int main () { return 0; }
