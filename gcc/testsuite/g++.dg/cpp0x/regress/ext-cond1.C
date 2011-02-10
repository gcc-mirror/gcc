// PR c++/12515
// { dg-do compile }
// { dg-options "-std=gnu++0x" }
template<int> void foo() { 0 ?: 0; }
