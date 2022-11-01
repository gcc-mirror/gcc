// PR c++/106304
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr106304 }

export module pr106304;

struct A { virtual ~A() = default; };
struct B : A { };

inline const B* as_b(const A& a) {
  return dynamic_cast<const B*>(&a);
}
