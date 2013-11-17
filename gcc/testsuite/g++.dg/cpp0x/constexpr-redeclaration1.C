// PR c++/59123
// { dg-do compile { target c++11 } }

// Fwd-declarations
struct S;
extern const S s;

// (... later) definitions
struct S {};
constexpr S s {};
