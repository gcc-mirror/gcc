// PR c++/97740
// { dg-do compile { target c++11 } }

struct A {
  constexpr const int* get() const { return &m; }
private:
  int m;
} a;

struct B { const int* p; };

template<class T>
void f() {
  constexpr B x = {a.get()}; // { dg-bogus "private" }
  constexpr B y = {&a.m};    // { dg-error "private" }
}

template void f<int>();
