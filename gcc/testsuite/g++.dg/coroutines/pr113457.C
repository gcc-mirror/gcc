// https://gcc.gnu.org/PR113457
namespace std {
template <typename _Tp, typename _Up = _Tp &&> _Up __declval(int);
template <typename _Tp> auto declval() noexcept -> decltype(__declval<_Tp>(0));
template <typename _Tp> struct remove_cv {
  using type = __remove_cv(_Tp);
};
template <typename _Tp> using remove_cv_t = typename remove_cv<_Tp>::type;
template <typename _Tp> struct remove_reference {
  using type = __remove_reference(_Tp);
};
template <typename _Tp>
using remove_reference_t = typename remove_reference<_Tp>::type;
template <typename _Tp> inline constexpr bool is_array_v = __is_array(_Tp);
template <typename _Tp> struct remove_cvref {};
namespace ranges {
} // namespace ranges
template <typename _Tp>
[[__nodiscard__]] constexpr typename std::remove_reference<_Tp>::type &&
move(_Tp &&__t) noexcept {
  return static_cast<typename std::remove_reference<_Tp>::type &&>(__t);
}
template <typename _Iterator> struct iterator_traits;
namespace __detail {
template <typename _Iter, typename _Tp> struct __iter_traits_impl {
  using type = iterator_traits<_Iter>;
};
template <typename _Iter, typename _Tp = _Iter>
using __iter_traits = typename __iter_traits_impl<_Iter, _Tp>::type;
} // namespace __detail
template <typename> struct indirectly_readable_traits {};
namespace __detail {
template <typename _Tp>
using __iter_value_t =
    typename __iter_traits<_Tp, indirectly_readable_traits<_Tp>>::value_type;
}
template <typename _Tp>
using iter_value_t = __detail::__iter_value_t<_Tp>;
namespace ranges::__access {
template <typename _Tp> auto __begin(_Tp &__t) {
    return __t + 0;
}
} // namespace ranges::__access
namespace __detail {
template <typename _Tp>
using __range_iter_t =
    decltype(ranges::__access::__begin(std::declval<_Tp &>()));
}
template <typename _Tp> struct iterator_traits<_Tp *> {
  using value_type = remove_cv_t<_Tp>;
};
  namespace ranges {
  namespace __access {
  struct _Begin {
    template <typename _Tp>
    constexpr auto operator() [[nodiscard]] (_Tp &&__t) const
	{
      if constexpr (is_array_v<remove_reference_t<_Tp>>)
      {
        return __t + 0;
      }
    }
  };
  } // namespace __access
  inline namespace _Cpo {
  inline constexpr ranges::__access::_Begin begin{};
  } // namespace _Cpo
  template <typename _Tp>
  concept range = requires(_Tp &__t) { ranges::begin(__t); };
  template <typename _Tp> using iterator_t = std::__detail::__range_iter_t<_Tp>;
  template <range _Range>
  using range_value_t = iter_value_t<iterator_t<_Range>>;
  template <range _Range>
  struct elements_of {
    _Range range;
  };
  template <typename _Range>
  elements_of(_Range &&) -> elements_of<_Range &&>;
  } // namespace ranges
  inline namespace __n4861 {
  template <typename _Result, typename = void>
  struct __coroutine_traits_impl {};
  template <typename _Result> struct __coroutine_traits_impl<_Result, void> {
    using promise_type = typename _Result::promise_type;
  };
  template <typename _Result, typename... _ArgTypes>
  struct coroutine_traits : __coroutine_traits_impl<_Result> {};
  template <typename _Promise = void> struct coroutine_handle;
  template <typename _Promise> struct coroutine_handle {
    constexpr void *address() const noexcept { return _M_fr_ptr; }
    constexpr static coroutine_handle from_address(void *__a) noexcept {
      coroutine_handle __self;
      return __self;
    }
    constexpr operator coroutine_handle<>() const noexcept {
      return coroutine_handle<>::from_address(address());
    }
    void *_M_fr_ptr = nullptr;
  };
  struct suspend_always {
    constexpr bool await_ready() const noexcept { return false; }
    constexpr void await_suspend(coroutine_handle<>) const noexcept {}
    constexpr void await_resume() const noexcept {}
  };
  } // namespace __n4861
  template <typename _Ref, typename _V = void>
  class generator;
  namespace __gen {
  template <typename _Yielded> class _Promise_erased {
    template <typename _Gen> struct _Recursive_awaiter;
    struct _Final_awaiter;
  public:
    suspend_always initial_suspend() const noexcept;
    suspend_always yield_value(_Yielded __val) noexcept;
    template <typename _R2, typename _V2>
    auto yield_value(
        ranges::elements_of<generator<_R2, _V2> &&> __r) noexcept {
      return _Recursive_awaiter{std::move(__r.range)};
    }
    template <typename _R>
    auto yield_value(ranges::elements_of<_R> __r) noexcept {
      auto __n = []( ranges::iterator_t<_R> __i)
          -> generator<_Yielded, ranges::range_value_t<_R>>
	  {
        co_yield static_cast<_Yielded>(*__i);
      };
      return
      yield_value
      (
      ranges::elements_of
      (
      __n
      (  ranges::begin(__r.range))));
    }
    _Final_awaiter final_suspend() noexcept;
    void unhandled_exception() {}
  };
  template <typename _Yielded>
  struct _Promise_erased<_Yielded>::_Final_awaiter {
    bool await_ready() noexcept;
    template <typename _Promise>
    void await_suspend(std::coroutine_handle<_Promise> __c) noexcept {}
    void await_resume() noexcept {}
  };
  template <typename _Yielded>
  template <typename _Gen>
  struct _Promise_erased<_Yielded>::_Recursive_awaiter {
    _Gen _M_gen;
    constexpr bool await_ready() const noexcept { return false; }
    template <typename _Promise>
    void
    await_suspend(std::coroutine_handle<_Promise> __p) noexcept {}
    void await_resume() {}
  };
  } // namespace __gen
  template <typename _Ref, typename _V>
  struct generator
  {
    struct promise_type
    :   __gen::_Promise_erased<const int &>
    {
      generator get_return_object() noexcept;
    };
  };
} // namespace std
using namespace std;
template <typename... Ranges>
auto concat(Ranges &&...ranges) -> generator<double, double> {
  (
  co_yield
  ranges::elements_of(ranges), ...);
}
auto main() -> int {
  int const numbers1[] = {4, 8, 15, 16, 23, 42};
  double const numbers2[] = {4, 8, 15, 16, 23, 42};
  concat(numbers1, numbers2)
  ;
}
