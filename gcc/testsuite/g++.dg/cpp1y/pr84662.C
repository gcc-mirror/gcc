// PR c++/84662
// { dg-do compile { target c++14 } }
// { dg-options "" }

double b;
a (__attribute__((c (0 && int() - ([] {} && b) || auto))));	// { dg-error "expected constructor, destructor, or type conversion before" }
