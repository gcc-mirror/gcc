// PR c++/97740
// { dg-do compile { target c++14 } }

struct A {
  constexpr const int* get() const { return &m; }
private:
  int m;
} a;

struct B { const int* p; };

template<A* arg>
void f() {
  [] (auto) {
    constexpr B x = {arg->get()}; // { dg-bogus "private" }
    constexpr B y = {&arg->m};    // { dg-error "private" }
  }(0);
}

template void f<&a>();
