// PR c++/105322
// { dg-module-do link }
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr105322.Decltype }

export module pr105322.Decltype;

auto f() {
  struct A { int m;
    int get () { return m; }
  };
  return A{};
}

export
inline void g1() {
  auto r = decltype(f()){0};
}

export
inline void g2() {
  auto r = f().m;
}

export
inline void g3() {
  auto r = f().get();
}
