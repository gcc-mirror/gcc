// PR c++/30274

struct S {
  bool x : 4;
};

S s;

void f() {
  s.x--; // { dg-error "Boolean expression" }
  --s.x; // { dg-error "Boolean expression" }
}
