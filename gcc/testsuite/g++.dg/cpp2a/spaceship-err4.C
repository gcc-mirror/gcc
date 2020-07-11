// PR c++/94478 - ICE with defaulted comparison operator.
// { dg-do compile { target c++20 } }

struct B {};
bool operator!=(const B&, const B&) = default; // { dg-error "not a friend" }
bool operator==(const B&, const B&) = default; // { dg-error "not a friend" }
bool operator<=>(const B&, const B&) = default; // { dg-error "not a friend" }
