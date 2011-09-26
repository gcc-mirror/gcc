// PR c++/50512

void foo (const char *const& s);
template<typename C> void foo (const C& x) { x.a; }

void f () {
  foo ("abc");
}
