// PR c++/38655
// { dg-options "" }

__decltype(0r)* p = 1; // { dg-error "not supported|invalid" }
