// { dg-do assemble  }

// gcc represents non-ellipsis parmlists by terminating them with
// a void parm. We need to distinguish between a parmlist of (void), and
// some ill-formed ones.

struct S; // { dg-error "" } forward ref

void f(S);            // ok
void f(S s) {}        // { dg-error "" } incomplete type
void j (int){}        // ok
void k (){}           // ok
void q (void){}       // ok
void t (void t);      // { dg-error "" } incomplete
void r (void, ...);   // { dg-error "" } incomplete
void s (void const);  // { dg-error "" } incomplete
