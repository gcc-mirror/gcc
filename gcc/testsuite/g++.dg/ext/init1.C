// PR c++/9623
// Test for trivial use of named initializer extension
// { dg-options "" }

struct S { int x;  int y; };
S s = { x:1, y:2 };
