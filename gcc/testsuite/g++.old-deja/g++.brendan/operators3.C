// Build don't link: 
// GROUPS passed operators
class X { };
void operator->(X& a, X& b) {} // MUST be a member function// ERROR - .*
