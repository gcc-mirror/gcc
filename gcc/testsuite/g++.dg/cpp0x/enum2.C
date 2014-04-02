// PR c++/38637
// { dg-do compile { target c++11 } }

template<int> enum E : int { e };	// { dg-error "declaration|expected" }
