// PR c++/105440
// { dg-do compile { target c++20 } }

struct basic_string {
  char _M_local_buf[32];
  char* _M_p;
  constexpr basic_string() : _M_p{_M_local_buf} { }
  constexpr void f() { if (_M_p) { } }
  constexpr ~basic_string() { if (_M_p) { } }
};

template<int N>
struct array {
  basic_string _M_elems[N];
};

constexpr basic_string get() { return {}; }

constexpr bool f1() {
  array<1> a{get()};
  a._M_elems[0].f();

  return true;
}

constexpr bool f2() {
  array<2> a2{get(), get()};
  array<3> a3{get(), get(), get()};

  for (basic_string& e : a2._M_elems)
    e.f();
  for (basic_string& e : a3._M_elems)
    e.f();

  return true;
}

static_assert(f1());
static_assert(f2());
