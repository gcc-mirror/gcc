// { dg-do assemble  }
// GROUPS passed niklas nested-types
struct A { struct B { void operator = (const B&); }; };
void A::B::operator = (const B&) {}
