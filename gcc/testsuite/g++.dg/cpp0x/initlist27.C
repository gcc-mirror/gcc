// PR c++/42061
// { dg-do compile }
// { dg-options "-std=c++11" }

int& i = { j };	// { dg-error "invalid initialization|was not declared" }
