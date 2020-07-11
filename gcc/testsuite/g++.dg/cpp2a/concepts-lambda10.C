// PR c++/94128
// { dg-do compile { target c++20 } }

void test(auto param)
requires requires{ { [](auto p){return p;}(param) }; };

void test2() { test(1); }
