// PR c++/15025

template <int> struct X; 
struct X {}; // { dg-error "" }
