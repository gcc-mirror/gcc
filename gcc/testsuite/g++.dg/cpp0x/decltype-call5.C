// PR c++/95675
// { dg-do compile { target c++11 } }

struct b {};
b operator|(b, b) { return {}; }
b e, f, g;
using h = decltype(e | f | g);
