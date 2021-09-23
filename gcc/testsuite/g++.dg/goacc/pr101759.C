// PR c++/101759
// { dg-do compile { target c++11 } }

#pragma acc routine
int foo (int x = []() { extern int bar (int); return 1; }());
