// PR120273
// { dg-additional-options "-Wno-literal-suffix" }
namespace std {
void declval();
template < typename > struct invoke_result;
template < typename _Fn > using invoke_result_t = invoke_result< _Fn >;
template < typename _Derived, typename _Base >
concept derived_from = __is_base_of(_Base, _Derived);
template < typename, typename >
concept convertible_to = requires { declval; };
template < char... > int operator""ms();
template < typename _Result, typename > struct coroutine_traits : _Result {};
template < typename = void > struct coroutine_handle {
  static coroutine_handle from_address(void *);
  operator coroutine_handle<>();
  void *address();
};
}

using namespace std;

template < class > using CoroutineHandle = coroutine_handle<>;

template < class Callable >
  requires(derived_from< invoke_result_t< Callable >, int >)
Callable operator co_await(Callable);

struct FinalSuspendProxy {
  bool await_ready() noexcept;
  void await_suspend(CoroutineHandle< void >) noexcept ;
  void await_resume() noexcept;
};

struct Task {
  struct Promise;
  using promise_type = Promise;

  struct Promise {
    auto initial_suspend() { return FinalSuspendProxy(); }
    auto final_suspend () noexcept  { return FinalSuspendProxy(); }
    void unhandled_exception () {}
    Task get_return_object () { return {}; }
  };
} ;

struct TestEventLoop {
  struct Sleep {
    Sleep(TestEventLoop, int);
    bool await_ready();
    void await_suspend(CoroutineHandle< void >);
    void await_resume();
  };
  auto sleep(int tm) { return Sleep(*this, tm); }
};

Task test_body_11(TestEventLoop t) {
  co_await t.sleep(5ms);
}
