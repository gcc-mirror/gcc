// PR c++/86546

class a b;  // { dg-error "aggregate" }
void c() {
  switch ()  // { dg-error "expected" }
  case b  // { dg-error "expected" }
