// PR c++/84662
// { dg-do compile { target c++14 } }

double b;
a (__attribute__((c (0 && int() - ([] {} && b) || auto))));	// { dg-error "auto|expected constructor, destructor, or type conversion before" }
