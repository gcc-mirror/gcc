// { dg-do assemble  }
// GROUPS passed operators
class X { };
void operator->(X& a, X& b) {} // MUST be a member function// { dg-error "" } .*
