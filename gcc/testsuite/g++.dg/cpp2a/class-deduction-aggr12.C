// PR c++/101803
// { dg-do compile { target c++20 } }

struct Inner { int i = 0; };

template <typename T = void>
struct Outer { Inner s{}; };

Outer o1{ .s = {} };                // works
Outer o2{ .s = Inner{ .i = 1} };    // works
Outer o3{ .s = { .i = 1} };         // does not

Outer o4{ .s{} };                   // works
Outer o5{ .s{Inner{ .i = 1} } };    // works
Outer o6{ .s{ .i = 1} };            // does not
