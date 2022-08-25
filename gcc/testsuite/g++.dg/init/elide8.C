// CWG 2403 case 3: we can't elide this copy because the delegating constructor
// might be used to initialize a base.
// { dg-do compile { target c++11 } }

struct Noncopyable {
  Noncopyable() = default;
  Noncopyable(const Noncopyable &) = delete;
  Noncopyable(int) : Noncopyable(make()) {} // { dg-error "deleted" }

  static Noncopyable make();
};
