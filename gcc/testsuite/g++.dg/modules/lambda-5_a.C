// PR c++/105322
// { dg-module-do link }
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr105322.Lambda }

export module pr105322.Lambda;

struct A { };

export
inline void f1() {
  A a;
  auto g1 = [a] { }; // used to ICE here during stream out
}

export
template<class...>
void f2() {
  A a;
  auto g2 = [a] { };
}

export
inline auto g3 = [a=A{}] { };
