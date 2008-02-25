// PR c++/35338
// { dg-options "" }

int i = 0r; // { dg-error "unnamed-fixed" }
bool b = !0r; // { dg-error "0.0|argument" }
