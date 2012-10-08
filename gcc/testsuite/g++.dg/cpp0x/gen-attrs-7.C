// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-parameter" }

void f (int i [[gnu::__unused__]]) {}
