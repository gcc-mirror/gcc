// Build don't link: 
// GROUPS niklas uncaught default-construct
struct A { A (int); };
struct B : A {}; // ERROR - without ctor // ERROR - candidates
void f () { B (0); }// ERROR - .*
