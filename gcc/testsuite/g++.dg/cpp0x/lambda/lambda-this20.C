// PR c++/63601
// { dg-do compile { target c++11 } }

auto f = []{ sizeof(this); };	// { dg-error "this" }
