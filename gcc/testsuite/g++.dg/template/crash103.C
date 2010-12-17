// PR c++/45665

template < typename > struct S;
void (S <0>::*ptr) (); // { dg-error "type" }
