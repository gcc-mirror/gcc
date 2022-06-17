// { dg-additional-options "-fpreprocessed -std=gnu++20 -w -fconcepts" }
int _M_invoke___functor;
namespace std {
template <int __v> struct integral_constant {
  static constexpr bool value = __v;
};
template <typename> struct decay { typedef int type; };
template <bool, typename> struct enable_if;
template <typename _Tp> struct enable_if<true, _Tp> { typedef _Tp type; };
template <typename, typename _Fn, typename... _Args>
void __invoke_impl(_Fn __f, _Args... __args) {
  __f(__args...);
}
template <typename, typename _Callable, typename... _Args>
void __invoke_r(_Callable __fn, _Args... __args) {
  using __result = int;
  using __type = __result;
  __invoke_impl<__type>(__fn, __args...);
}
template <typename> class function;
template <typename _Functor> struct _Base_manager {
  static _Functor *_M_get_pointer(int) {}
};
template <typename, typename> class _Function_handler;
template <typename _Res, typename _Functor, typename... _ArgTypes>
struct _Function_handler<_Res(_ArgTypes...), _Functor> {
  static _Res _M_invoke(_ArgTypes... __args) {
    auto __trans_tmp_1 =
        _Base_manager<_Functor>::_M_get_pointer(_M_invoke___functor);
    __invoke_r<_Res>(*__trans_tmp_1, __args...);
  }
};
template <typename _Res, typename... _ArgTypes>
struct function<_Res(_ArgTypes...)> {
  template <typename _Functor> function(_Functor) {
    _Function_handler<_Res(_ArgTypes...), _Functor>::_M_invoke;
  }
};
template <typename _Tp> struct __shared_ptr_access {
  using element_type = _Tp;
  element_type *operator->();
};
template <typename> void make_shared();
} // namespace std
namespace boost {
using std::decay;
using std::enable_if;
using std::integral_constant;
namespace asio {
namespace detail {
template <typename> struct is_completion_signature;
template <typename R, typename... Args>
struct is_completion_signature<R(Args...)> : integral_constant<true> {};
} // namespace detail
template <typename T>
concept completion_signature = detail::is_completion_signature<T>::value;
template <typename...> struct async_result;
namespace detail {
template <completion_signature>
struct async_result_has_initiate_memfn : integral_constant<1> {};
} // namespace detail
template <typename CompletionToken, completion_signature... Signatures,
          typename Initiation>
enable_if<detail::async_result_has_initiate_memfn<Signatures...>::value,
          decltype(async_result<typename decay<CompletionToken>::type,
                                Signatures...>::initiate(0))>::type
async_initiate(Initiation) {}
} // namespace asio
} // namespace boost
namespace malloy::websocket {
struct connection {
  auto read(auto, auto done) {
    auto wrapper = [] {};
    return boost::asio::async_initiate<decltype(done), void()>(wrapper);
  }
};
} // namespace malloy::websocket
namespace std {
template <typename...> struct coroutine_traits;
template <typename = void> struct coroutine_handle {
  operator coroutine_handle<>();
};
struct suspend_always {
  bool await_ready();
  void await_suspend(coroutine_handle<>);
  void await_resume();
};
} // namespace std
namespace boost {
namespace asio {
namespace detail {
using std::coroutine_handle;
using std::suspend_always;
template <typename> class awaitable_frame;
} // namespace detail
template <typename, typename = int> struct awaitable {
  bool await_ready();
  template <class U>
  void await_suspend(detail::coroutine_handle<detail::awaitable_frame<U>>);
  void await_resume();
};
namespace detail {
struct awaitable_frame_base {
  auto initial_suspend() { return suspend_always(); }
  auto final_suspend() noexcept {
    struct result {
      bool await_ready() noexcept;
      void await_suspend(coroutine_handle<>) noexcept;
      void await_resume() noexcept;
    };
    return result{};
  }
  void unhandled_exception();
  template <typename T> auto await_transform(T a) { return a; }
};
template <> struct awaitable_frame<void> : awaitable_frame_base {
  void get_return_object();
};
} // namespace detail
} // namespace asio
} // namespace boost
namespace std {
template <typename T, typename Executor, typename... Args>
struct coroutine_traits<boost::asio::awaitable<T, Executor>, Args...> {
  typedef boost::asio::detail::awaitable_frame<T> promise_type;
};
} // namespace std
namespace boost {
namespace asio {
struct use_awaitable_t {
  use_awaitable_t(char, int, char);
} use_awaitable(0, 0, 0);
template <typename Executor, typename R, typename... Args>
struct async_result<Executor, R(Args...)> {
  template <typename Initiation> static awaitable<int> initiate(Initiation);
};
} // namespace asio
} // namespace boost
namespace malloy::test {
void roundtrip_coro(std::function<void(int)>);
}
using boost::asio::awaitable;
template <int> awaitable<void> client_ws_handler_coro() {
  std::__shared_ptr_access<malloy::websocket::connection> conn;
  auto buffer = std::make_shared<int>;
  co_await conn->read(buffer, boost::asio::use_awaitable);
}
void port() {
  malloy::test::roundtrip_coro([](auto) { client_ws_handler_coro<false>; });
}
