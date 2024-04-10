// PR c++/107291
// { dg-do compile { target c++20 } }

struct S4;					   // { dg-message "declared here" }
bool operator==(S4 const &, S4 const &) = default; // { dg-error "not a friend" }
