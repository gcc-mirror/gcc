// Build don't link: 
// GROUPS passed initialization
// ARM $11.4: A function first declared in a friend decl is equivalent
// to an extern decl, so the below is illegal.

class X {
      friend g(); // ERROR - previous declaration
};
static g() { return 1; }// ERROR - previously declared
