// Build don't link:

// gcc represents non-ellipsis parmlists by terminating them with
// a void parm. We need to distinguish between a parmlist of (void), and
// some ill-formed ones.

struct S; // ERROR - forward ref

void f(S);            // ok
void f(S s) {}        // ERROR - incomplete type
void j (int){};       // ok
void k (){};          // ok
void q (void){}       // ok
void t (void t);      // ERROR - incomplete
void r (void, ...);   // ERROR - incomplete
void s (void const);  // ERROR - incomplete
