// PR c++/35338
// { dg-options "" }

int i = 0r; // { dg-error "not supported" }
bool b = !0r; // { dg-error "not supported" }
