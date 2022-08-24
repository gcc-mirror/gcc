// PR c++/104425
// { dg-do compile { target c++11 } }

namespace A { class foo {}; }
namespace B { class bar {}; }

A::foo& operator<<(A::foo& f, const B::bar&);

namespace C {
  template<class T> T val();

  A::foo& operator<<(A::foo& f, int in);

  template<class T, class = void>
  struct has_insertion_operator {
    static constexpr bool value = false;
  };

  template<class T>
  struct has_insertion_operator<T, decltype(val<A::foo&>() << val<T>(), void())> {
    static constexpr bool value = true;
  };
}

static_assert(!C::has_insertion_operator<B::bar>::value, "");
