// PR c++/42061
// { dg-do compile { target c++11 } }

int& i = { j };	// { dg-error "invalid initialization|was not declared" }
