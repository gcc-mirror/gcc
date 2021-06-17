// PR c++/101106
// { dg-do compile { target c++11 } }
// { dg-options "" }

int f();		// { dg-bogus "previous declaration" }
int f() = delete;
