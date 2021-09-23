// PR c++/101106
// { dg-do compile { target c++11 } }
// { dg-options "" }

int f();
int f() = delete;		// { dg-message "not first declaration" }
