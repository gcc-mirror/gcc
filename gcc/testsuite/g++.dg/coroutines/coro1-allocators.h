/* Include <coroutine> or the equivalent.  */
#include "coro.h"

/* Allow for stand-alone testing with no headers installed.  */
#if __has_include(<new>)
#  include <new>
#else

/* Required when get_return_object_on_allocation_failure() is defined by
  the promise.  We need a no-throw new, and new etc.  build the relevant
  pieces here to avoid needing the headers in the test.  */

namespace std {
  struct nothrow_t {};
  constexpr nothrow_t nothrow = {};
  typedef __SIZE_TYPE__ size_t;
} // end namespace std

void* operator new(std::size_t, const std::nothrow_t&) noexcept;
void  operator delete(void* __p, const std::nothrow_t&) noexcept;

#endif

/* Flags and counters so that we can test that the methods we expected
   to be called, were called (and the correct number of times).  */

#ifdef USE_FAILING_OP_NEW
extern int used_failing_new;
#endif

#if defined (PROVIDE_NEW_SZT) || defined (PROVIDE_NEW_SZT_INT)
extern int used_ovl_new;
#endif

#ifdef PROVIDE_NEW_SZT_NT
extern void *malloc (size_t);
extern int used_ovl_new_nt;
#endif

#ifdef PROVIDE_DEL_VP
extern int used_ovl_del;
#endif

#ifdef PROVIDE_DEL_VP_SZT
extern int used_ovl_del_2arg;
#endif

#ifdef PROVIDE_GROOAF
extern int used_grooaf;
#endif

struct coro1 {
  struct promise_type;
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  handle_type handle;
  coro1 () noexcept : handle(0) {}
  coro1 (handle_type _handle) noexcept
    : handle(_handle)  {
        PRINT("Created coro1 object from handle");
  }
  coro1 (const coro1 &) = delete; // no copying
  coro1 (coro1 &&s) noexcept : handle(s.handle)  {
	s.handle = nullptr;
	PRINT("coro1 mv ctor ");
  }
  coro1 &operator = (coro1 &&s) noexcept {
	handle = s.handle;
	s.handle = nullptr;
	PRINT("coro1 op=  ");
	return *this;
  }
  ~coro1() noexcept {
        PRINT("Destroyed coro1");
        if ( handle )
          handle.destroy();
  }

  struct suspend_never_prt {
  bool await_ready() const noexcept { return true; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-never-susp");}
  void await_resume() const noexcept { PRINT ("susp-never-resume");}
  ~suspend_never_prt() {};
  };

  struct  suspend_always_prt {
  bool await_ready() const noexcept { return false; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
  void await_resume() const noexcept { PRINT ("susp-always-resume");}
  };

  struct promise_type {
  promise_type() {  PRINT ("Created Promise"); }
  ~promise_type() { PRINT ("Destroyed Promise"); }

  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }
  auto initial_suspend () {
    PRINT ("get initial_suspend (always)");
    return suspend_always_prt{};
  }
  auto final_suspend () {
    PRINT ("get final_suspend (always)");
    return suspend_always_prt{};
  }
  void return_void () {
    PRINT ("return_void ()");
  }
  void unhandled_exception() { PRINT ("** unhandled exception"); }

#ifdef USE_FAILING_OP_NEW
  /* Provide an operator, that always fails.  */
  void *operator new (std::size_t sz) noexcept {
    PRINT ("promise_type: used failing op new");
    used_failing_new++;
    return nullptr;
  }
#endif

#ifdef PROVIDE_NEW_SZT
  void *operator new (std::size_t sz) {
    PRINT ("promise_type: used overloaded operator new");
    used_ovl_new++;
    return ::operator new(sz);
  }
#endif

#ifdef PROVIDE_NEW_SZT_NT
  void *operator new (std::size_t sz, const std::nothrow_t&) noexcept {
    PRINT ("promise_type: used overloaded operator new NT");
    return malloc (sz);
  }
#endif

#ifdef PROVIDE_NEW_SZT_INT
  void *operator new (std::size_t sz, int x) {
    PRINT ("promise_type: used overloaded operator new with int arg");
    used_ovl_new += x;
    return ::operator new(sz);
  }
#endif

#ifdef PROVIDE_DEL_VP
  void operator delete (void *p) {
    PRINT ("promise_type: used overloaded operator delete 1 arg");
    used_ovl_del++;
    return ::operator delete(p);
  }
#endif

#ifdef PROVIDE_DEL_VP_SZT
  void operator delete (void *p, std::size_t sz) {
    PRINT ("promise_type: used overloaded operator delete 2 args");
    used_ovl_del_2arg++;
    return ::operator delete(p);
  }
#endif

#ifdef BOGUS_OPNEW_CASE1
  /* Provide an operator, but it doesn't match on overload.  */
  void *operator new (std::size_t sz, char *f) noexcept {
    PRINT ("promise_type: used bogus op new");
    return nullptr;
  }
#endif

#ifdef BOGUS_OPDEL_CASE1
  /* Provide an operator, but it doesn't match on overload.  */
  void operator delete (void *p, char *f) {
    PRINT ("promise_type: used bogus overloaded operator delete");
  }
#endif

#ifndef BAD_GROOAF_STATIC
# define BAD_GROOAF_STATIC static
#endif
#ifdef PROVIDE_GROOAF
  BAD_GROOAF_STATIC coro1 get_return_object_on_allocation_failure () noexcept {
    PRINT ("alloc fail return");
    used_grooaf++;
    return coro1 (nullptr);
  }
#endif

  }; // promise
}; // coro1
