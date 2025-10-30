// PR c++/121574
// { dg-additional-options "-fmodules -Wno-error=expose-global-module-tu-local -Wtemplate-names-tu-local -Wno-global-module" }
// { dg-module-cmi M }

module;

namespace {
  void foo() {}
  inline int bar = 123;
  template <typename> void qux() {}
  template void qux<int>();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wexpose-global-module-tu-local"
  void foo_ignored() {}
  inline int bar_ignored = 123;
  template <typename> void qux_ignored() {}
  template void qux_ignored<int>();
#pragma GCC diagnostic pop
};

export module M;

export inline void a() {  // { dg-warning "exposes TU-local" }
  foo();
  int x = bar;
}

export inline void b() {
  foo_ignored();
  int x = bar_ignored;
}

export template <typename T>
void c() {  // { dg-warning "refers to TU-local" }
  foo();
  int x = bar;
}

export template <typename T>
void d() {
  foo_ignored();
  int x = bar_ignored;
}

export inline void e() {  // { dg-warning "exposes TU-local" }
  foo();
  int result = bar_ignored;
}

export template <typename T>
void f() {  // { dg-warning "refers to TU-local" }
  foo_ignored();
  int result = bar;
}

export inline void g() {  // { dg-warning "exposes TU-local" }
  qux<int>();
}

export template <typename>
void h() {
  qux_ignored<int>();
}
