// PR c++/94478 - ICE with defaulted comparison operator.
// { dg-do compile { target c++2a } }

struct B {};
bool operator!=(const B&, const B&) = default; // { dg-error "equality comparison operator can only be defaulted in a class definition" }
bool operator==(const B&, const B&) = default; // { dg-error "equality comparison operator can only be defaulted in a class definition" }
bool operator<=>(const B&, const B&) = default; // { dg-error "three-way comparison operator can only be defaulted in a class definition" }
