// PR c++/128970
// { dg-do compile }

struct S { int b : 3; } s = {};
int foo () { return 1; }
int bar () { return (foo () ? s.b : s.b) > 0; }
int main () { return bar (); }
