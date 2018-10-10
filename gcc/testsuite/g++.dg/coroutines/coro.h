
#ifndef __CORO_TESTSUITE_HEADER_A
#define __CORO_TESTSUITE_HEADER_A

// Fragments (with short-cuts) to mimic enough of the library header to
// make some progress.

namespace std {
namespace experimental {
inline namespace coroutines_n4760 {

// 21.11.1 coroutine traits
template<typename R, typename...> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};

// 21.11.2  coroutine handle
template <typename Promise = void> struct coroutine_handle;

template <> struct coroutine_handle<void> {
    protected:
      // shown as explicit type, could move to simple
      // void pointer when __builtin_coro_X are implemented
      struct __resume_data {
	// the coroutine resumer (let's cross the streams).
	void (*__resume) (__resume_data *p);
	// (Gozer) the destructor.
	void (*__destroy) (__resume_data *p);
	// -1: unstarted
	//  0: final-suspend
	// N>0: other suspend-point
        int __resume_at;
      };
    public:
      // 21.11.2.1 construct/reset
      constexpr coroutine_handle () noexcept
		: __rd (0) {}
      constexpr coroutine_handle (decltype(nullptr) __rd) noexcept
		: __rd (__rd) {}
      coroutine_handle &operator= (decltype(nullptr) __rd) noexcept {
	this->__rd = __rd; return *this;
      }

    public:
     // 21.11.2.2 export/import
      constexpr void *address () const noexcept {
        return __rd;
      }
      constexpr static coroutine_handle from_address (void *addr) noexcept {
        coroutine_handle __self;
	__self.__rd = static_cast<struct __resume_data *>(addr);
	return __self;
        // return coroutine_handle (__rd);
      }
    public:
      // 21.11.2.3 observers
      constexpr explicit operator bool () const noexcept {
        return bool (__rd);
      }
      bool done () const noexcept {
	// __builtin_coro_done (__rd)
	return !__rd->__resume_at;
      }
      // 21.11.2.4 resumption
      void operator () () const { resume (); }
      void resume () const {
	// __builtin_coro_resume (__rd)
	__rd->__resume (__rd);
      }
      void destroy () const {
	// __builtin_coro_destroy (__rd)
	__rd->__destroy (__rd);
      }
    protected:
      __resume_data *__rd;
};

template <class Promise>
struct coroutine_handle : coroutine_handle<> {
  // 21.11.2.1 construct/reset
  using coroutine_handle<>::coroutine_handle;
  static coroutine_handle from_promise(Promise &p){
    // __builtin_coro_from_promise
    coroutine_handle __self;
    __self.__rd = reinterpret_cast<struct __resume_data *>
                  ((char *)&p - sizeof (__resume_data));
    return __self;
  }
  coroutine_handle& operator=(decltype(nullptr)) noexcept;
  // 21.11.2.2 export/import
  constexpr static coroutine_handle from_address(void* addr){
    coroutine_handle __self;
    __self.__rd = static_cast<struct __resume_data *>(addr);;
    return __self;
  }
  // 21.11.2.5 promise access
  Promise& promise() const {
    char *t = (char *)this + sizeof (__resume_data);
    return reinterpret_cast<Promise &>(t);
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

}}} // namespace std::experimental::coroutines_n4760

namespace coro = std::experimental::coroutines_n4760;

struct Coro {
  coro::coroutine_handle<> handle;
  Coro (coro::coroutine_handle<> handle) : handle (handle) {}
  struct Promise {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    Coro get_return_object() {
      return Coro (coro::coroutine_handle<Promise>::from_promise (*this));
    }
    void return_void() {};
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<Coro> {
    using promise_type = Coro::Promise;
};

/* Diagose missing return_void() in the promise type.  */
struct MissingRetVoid {
  coro::coroutine_handle<> handle;
  MissingRetVoid (coro::coroutine_handle<> handle) : handle (handle) {}
  struct missing_retvoid {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    MissingRetVoid get_return_object() {
      return MissingRetVoid (coro::coroutine_handle<missing_retvoid>::from_promise (*this));
    }
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<MissingRetVoid> {
    using promise_type = MissingRetVoid::missing_retvoid;
};

#endif
