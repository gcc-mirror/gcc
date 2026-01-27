// { dg-lto-do assemble }
// { dg-lto-options { { -flto -ffat-lto-objects -g } } }

template <typename T> void foo (void *, void *) { new T; }
using C = void (*) (void *, void *);
template <typename T> struct D { static constexpr C foo = ::foo <T>; };
struct E { void (*bar) (void *, void *); };
constexpr bool v = false;
template <typename T, typename> void baz () { E { D<T>::foo }; }
template <class, class, bool> struct F;
template <class R, class E> struct F <R, E, false> { static void qux () { baz <R, E> (); } };
template <typename... T> void corge () { (F <T, void, v>::qux (), ...); }
template <auto S> struct G { long long val = 0; };
struct H {
  virtual void garply ();
  void plugh (const int &);
  G <&H::plugh> h;
};
void fred () { corge <H> (); }
