// PR c++/50391
// { dg-do compile { target c++11 } }

namespace std
{
  template<typename T, T Val>
    struct integral_constant
    { static constexpr T value = Val; };

  template<typename T>
    struct is_abstract
    : integral_constant<bool, __is_abstract(T)>
    { };

  template<typename T, bool = is_abstract<T>::value>
    struct is_destructible
    : integral_constant<bool, true>
    { };

  template<typename T>
    struct is_destructible<T, true>
    : integral_constant<bool, false>
    { };

  template<typename T>
    struct is_nothrow_move_constructible
    : is_destructible<T>
    { };

  template<typename T>
    struct decay
    { typedef T type; };

  template<typename T>
    struct decay<T&>
    { typedef T type; };

} // std

template<class Tp>
  struct single
  {
    Tp elem;

    constexpr single(const Tp& e)
    : elem(e) { }

    single(single&& s)
    noexcept(std::is_nothrow_move_constructible<Tp>::value) 
    : elem(s.elem) { }
  };

template<class Tp>
  constexpr single<typename std::decay<Tp>::type>
  make_single(Tp&& x)
  {
    return single<typename std::decay<Tp>::type>(x);
  }

class Blob;  // { dg-message "forward declaration" }

void
foo(Blob *b)
{
  make_single(*b);
}

// { dg-excess-errors "incomplete type|not a member" }
