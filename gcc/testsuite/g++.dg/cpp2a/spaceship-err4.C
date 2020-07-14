// PR c++/94478 - ICE with defaulted comparison operator.
// { dg-do compile { target c++2a } }

struct B {};
bool operator!=(const B&, const B&) = default; // { dg-error "not a friend" }
bool operator==(const B&, const B&) = default; // { dg-error "not a friend" }
bool operator<=>(const B&, const B&) = default; // { dg-error "not a friend" }
