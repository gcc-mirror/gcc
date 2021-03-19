// PR c++/99500
// { dg-do compile { target c++20 } }

bool b = requires (bool a, int a) { requires true; }; // { dg-error "conflicting declaration" }
