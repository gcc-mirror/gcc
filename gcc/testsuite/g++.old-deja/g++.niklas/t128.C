// Build don't link: 
// GROUPS niklas uncaught default-construct
struct A { A (int); };
struct B : A {}; // ERROR - 
void f () { B (0); }// ERROR - .*
