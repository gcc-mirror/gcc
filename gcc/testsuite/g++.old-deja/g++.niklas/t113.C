// Build don't link: 
// GROUPS passed niklas explicit-construct virtual-base
struct A {};
struct B : virtual A { B(); };
struct C : B {};
struct D { D(C&); };
D d(C());
