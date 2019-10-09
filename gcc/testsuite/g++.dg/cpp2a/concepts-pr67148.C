// PR c++/67148
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

namespace std
{
  template<typename T>
  T declval();

  typedef unsigned int size_t;
  typedef int ptrdiff_t;
  typedef decltype(nullptr) nullptr_t;
  template<typename _Tp, _Tp... _Idx>
    struct integer_sequence
    {
      typedef _Tp value_type;
      static constexpr size_t size() { return sizeof...(_Idx); }
    };

  template <class T, T Value>
  struct integral_constant {
    using type = integral_constant;
    using value_type = T;
    constexpr operator T() const { return Value; }
    constexpr T operator()() const { return Value; }
    static constexpr T value {Value};
  };
  template <class T, T Value>
  constexpr T integral_constant<T, Value>::value;
  using true_type = integral_constant<bool, true>;
  using false_type = integral_constant<bool, false>;

  template <class T, class U>
  struct is_same : false_type {};
  template <class T>
  struct is_same<T,T> : true_type {};
}
       
namespace meta
{
    inline namespace v1
    {
        template <typename T>
        using _t = typename T::type;
        template <bool... Bools>
        using and_c = std::is_same<std::integer_sequence<bool, Bools...>,
                                   std::integer_sequence<bool, (Bools || true)...>>;
    }
}

namespace stl2 { inline namespace v1 {
using std::declval;
namespace detail {
template <class...>
struct all_same : std::true_type {};
template <class T, class...Rest>
struct all_same<T, Rest...> :
  meta::and_c<__is_same_as(T, Rest)...> {};
}
template <class...Ts>
concept bool Same() {
  return detail::all_same<Ts...>::value;
}
template <class F, class...Args>
using ResultType = decltype(declval<F>()(declval<Args>()...));
template <class>
struct value_type {};
template <class T>
struct value_type<T*> {
  using type = T;
};
template <class T>
using ValueType =
  typename value_type<T>::type;

template <class F, class...Args>
concept bool Function() {
  return requires (F& f, Args&&...args) {
    f((Args&&)args...);
    requires Same<decltype(f((Args&&)args...)), ResultType<F, Args...> >();
  };
}

template <class, class...> struct __function : std::false_type {};
Function{F, ...Args} struct __function<F, Args...> : std::true_type {};

template <class F, class I1, class I2>
concept bool IndirectCallable() {
  return Function<F, ValueType<I1>, ValueType<I2>>();
}

template <class F, class I1, class I2>
concept bool IndirectCallable2() {
  return __function<F, ValueType<I1>, ValueType<I2>>::value;
}

namespace ext { namespace models {
template <class, class, class>
constexpr bool indirect_callable() { return false; }
IndirectCallable{F, I1, I2}
constexpr bool indirect_callable() { return true; }

template <class, class, class>
constexpr bool indirect_callable2() { return false; }
IndirectCallable2{F, I1, I2}
constexpr bool indirect_callable2() { return true; }
}}
}}

namespace models = stl2::ext::models;

template <class T = void>
struct plus {
  T operator()(T, T) const;
};

static_assert((models::indirect_callable<::plus<int>, int*, int*>()));
static_assert((models::indirect_callable2<::plus<int>, int*, int*>()));

static_assert((models::indirect_callable<::plus<int>, int**, int*>())); // { dg-error "static assertion failed" }
static_assert((models::indirect_callable2<::plus<int>, int**, int*>())); // { dg-error "static assertion failed" }
