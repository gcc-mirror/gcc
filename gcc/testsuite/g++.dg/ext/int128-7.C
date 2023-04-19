// PR c++/108099
// { dg-do compile { target { c++11 && int128 } } }

using i128 = signed __int128_t;	// { dg-error "specified with" }
