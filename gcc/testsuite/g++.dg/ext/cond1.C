// PR c++/12515
// { dg-do compile }
// { dg-options "" }
template<int> void foo() { 0 ?: 0; }
