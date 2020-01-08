// PR c++/92973
// { dg-do compile { target c++11 } }

struct S { bool operator==(const S&) const = default; int s; };	// { dg-error "only available with" "" { target c++17_down } }
struct T { bool operator!=(const T&) const = default; int t; };	// { dg-error "only available with" "" { target c++17_down } }
