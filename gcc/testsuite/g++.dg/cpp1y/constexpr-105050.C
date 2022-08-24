// PR c++/105050
// { dg-do compile { target c++14 } }

void g();
void h();

constexpr void f(int* p, int* q) {
  if (p != q && *p < 0) // { dg-error "neither branch of 'if' is a constant expression" }
    g();
  else
    h();
}
