// { dg-options "-Weffc++" }

struct S {};
/* Base classes should have virtual destructors.  */
struct T : public S {}; // { dg-warning "" }
