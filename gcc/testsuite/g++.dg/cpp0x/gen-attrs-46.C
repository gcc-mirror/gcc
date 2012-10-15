// PR c++/40821
// { dg-do compile { target c++11 } }

struct [[gnu::aligned(8)] S1 { int i; }; // { dg-error "" }
struct  [aligned(8)  S2 { int i; }; // { dg-error "" }
