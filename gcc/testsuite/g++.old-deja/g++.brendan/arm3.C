// { dg-do assemble  }
// GROUPS passed initialization
// ARM $11.4: A function first declared in a friend decl is equivalent
// to an extern decl, so the below is illegal.

class X {
      friend g(); // { dg-error "" } previous declaration
};
static g() { return 1; }// { dg-error "" } previously declared
