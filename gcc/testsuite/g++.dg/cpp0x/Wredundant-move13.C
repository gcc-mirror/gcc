// PR c++/107363
// { dg-do compile { target c++11 } }
// { dg-options "-Wredundant-move" }

// Define std::move.
namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

template <typename T, typename U>
struct Optional {
  U &value();
  T release_value() {
    T t = std::move (value ());
    return t;
  }
};

struct Foo {};
void test(Optional<const Foo, const Foo> o) { o.release_value(); }

struct F {
  F(const F&);
  F(F&&) = delete;
};

struct Z {
  Z(const Z&) = delete;
  Z(Z&&) = delete;
  Z(const Z&&);
};

const F& constfref();
const Z& constzref();

void
g ()
{
  // Will call F::F(const F&) w/ and w/o std::move.  So it's redundant.
  F f = std::move (constfref()); // { dg-warning "redundant move in initialization" }
  (void) f;
  // Will call Z::Z(const Z&&) w/ std::move, and Z::Z(const Z&) w/o.
  // So it's not redundant.
  Z z = std::move (constzref());
  (void) z;
}
