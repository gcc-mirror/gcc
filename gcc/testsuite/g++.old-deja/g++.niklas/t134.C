// Build don't link: 
// GROUPS passed niklas static-members
extern "C" int f ();
struct A { static void f () {} };
