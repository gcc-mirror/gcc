// Build don't link: 
// GROUPS passed niklas explicit-construct
struct A { A(); };
struct B { B(A&); };
B b(A());
