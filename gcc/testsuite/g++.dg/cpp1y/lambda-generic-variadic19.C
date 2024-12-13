// PR c++/86728
// { dg-do compile { target c++14 } }

auto c = [](auto x ...) { };	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
