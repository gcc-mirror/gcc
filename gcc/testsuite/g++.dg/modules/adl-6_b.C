// PR c++/117658
// { dg-additional-options "-fmodules" }
// { dg-module-cmi N }

export module N;
import M;

export R::X make();

namespace R {
  static int g(X);
}

export
template<typename T, typename U>
void apply_ok(T t, U u) {
  f(t, u);
}

export
template<typename T>
void apply_err(T t) {
  g(t);
}

export I::Y make_Y();
