// PR c++/89571
// { dg-do compile { target c++11 } }

struct z8 {
  constexpr static int qq /* = 0 */;  // { dg-error "initializer" }
};

template<typename T>
struct kf {
  kf (const kf &) noexcept (T::qq);  // { dg-error "constant" }
};

struct lk {
  kf<z8> e1;
};

template<typename T>
T &sc ();

struct b6 {
  decltype (lk (sc<lk> ())) zz;
};
