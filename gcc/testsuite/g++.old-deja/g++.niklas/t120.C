// Build don't link: 
// GROUPS passed niklas ellipsis
typedef void (*T) (...);
void f ();
struct S { void g (T); void h() { g(f); } };// ERROR - 
