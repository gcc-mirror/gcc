// Build don't link: 
// GROUPS passed niklas nested-types
struct A { struct B { ~B (); }; };
A::B::~B () {}
