// Build don't link: 
// GROUPS passed niklas ellipsis
void f ();
void g1 (void (*) (...)); void h1 () { g1 (f); }// ERROR - .*
struct S { void g2 (void (*) (...)); void h2 () { g2 (f); } };// ERROR - 
