#ifndef __CORO_H_N4775
#define __CORO_H_N4775

// Fragments (with short-cuts) to mimic enough of the library header to
// make some progress.

namespace std {
namespace experimental {
inline namespace coroutines_n4775 {

// 21.11.1 coroutine traits
template<typename _R, typename...> struct coroutine_traits {
  using promise_type = typename _R::promise_type;
};

// 21.11.2  coroutine handle
template <typename Promise = void> struct coroutine_handle;

template <> struct coroutine_handle<void> {
  public:
      // 21.11.2.1 construct/reset
  constexpr coroutine_handle () noexcept
    : __handle (0) {}
  constexpr coroutine_handle (decltype(nullptr) __h) noexcept
    : __handle (__h) {}
  coroutine_handle &operator= (decltype(nullptr)) noexcept {
    __handle = nullptr;
    return *this;
  }

  public:
    // 21.11.2.2 export/import
    constexpr void *address () const noexcept { return __handle; }
    constexpr static coroutine_handle from_address (void *__a) noexcept {
      coroutine_handle __self;
      __self.__handle = __a;
      return __self;
    }
  public:
      // 21.11.2.3 observers
    constexpr explicit operator bool () const noexcept {
      return bool (__handle);
    }
    bool done () const noexcept {
      return __builtin_coro_done (__handle);
    }
      // 21.11.2.4 resumption
    void operator () () const { resume (); }
    void resume () const {
      __builtin_coro_resume (__handle);
    }
    void destroy () const {
      __builtin_coro_destroy (__handle);
    }
  protected:
    void *__handle;
};

template <class _Promise>
struct coroutine_handle : coroutine_handle<> {
  // 21.11.2.1 construct/reset
  using coroutine_handle<>::coroutine_handle;
  static coroutine_handle from_promise(_Promise &p) {
    coroutine_handle __self;
    __self.__handle = 
      __builtin_coro_promise((char *)&p,  __alignof(_Promise), true);
    return __self;
  }
  coroutine_handle& operator=(decltype(nullptr)) noexcept {
    coroutine_handle<>::operator=(nullptr);
    return *this;
  }
  // 21.11.2.2 export/import
  constexpr static coroutine_handle from_address(void* __a){
    coroutine_handle __self;
    __self.__handle = __a;
    return __self;
  }
  // 21.11.2.5 promise access
  _Promise& promise() const {
    void * __t = __builtin_coro_promise(this->__handle,
					__alignof(_Promise), false);
    return *static_cast<_Promise*>(__t);
  }
};

// n4760 - 21.11.5 trivial awaitables

struct suspend_always {
  bool await_ready() { return false; }
  void await_suspend(coroutine_handle<>) {}
  void await_resume() {}
};

struct suspend_never {
  bool await_ready() { return true; }
  void await_suspend(coroutine_handle<>) {}
  void await_resume() {}
};

}}} // namespace std::experimental::coroutines_n4775

#endif // __CORO_H_N4775
