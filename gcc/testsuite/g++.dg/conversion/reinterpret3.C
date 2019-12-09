struct S {};

S s;

void f() {
  reinterpret_cast<const S>(s); // { dg-error "3:invalid cast" }
}
