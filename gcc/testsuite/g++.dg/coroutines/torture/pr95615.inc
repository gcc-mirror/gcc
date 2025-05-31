#include <coroutine>
#include <exception>
#include <cstdio>
#include <cstdlib>

#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF(...)
#else
//#include <iostream>
#  define PRINT(X) puts(X)
//#  define PRINT(X) std::cout << (X) << std::endl
#  define PRINTF(...) printf(__VA_ARGS__)
#endif

int frame_live = 0;
int promise_live = 0;
int Y_live = 0;
int task_live = 0;
int unhandled_exception_called = 0;

struct X {};

struct Y
{
  Y () {
    PRINT("Y ()");
    Y_live++;
  } 
  Y (const Y&) {
    PRINT("Y (const Y&)");
    Y_live++;
  } 
  ~Y () {
    PRINT("~Y ()");
    --Y_live;
  }
};

struct task {
    struct promise_type;
    using coro_handle = std::coroutine_handle<promise_type>;
    struct Z {
      std::exception_ptr eptr;
      coro_handle h;
    };
    struct promise_type {
        void* operator new(size_t sz) {
            PRINT("operator new()");
            frame_live++;
            return ::operator new(sz);
        }

        static void operator delete(void* p, size_t sz)  {
            PRINT("operator delete");
            --frame_live;
            ::operator delete(p, sz);
        }

        promise_type() {
            PRINT("promise_type()");
#ifdef PROMISE_CTOR_THROWS
             throw X{};
#endif
            promise_live = true;
        }

        ~promise_type() {
            PRINT("~promise_type()");
            --promise_live;
        }

        struct awaiter {
            bool await_ready() {
             PRINT("initial await_ready()");
#ifdef INITIAL_AWAIT_READY_THROWS
               throw X{};
#endif
// If we are testing the suspend infra, then suspend, otherwise continue
// so that we can test later paths (effectively, suspend_never).
#ifdef INITIAL_AWAIT_SUSPEND_THROWS
               return false;
#else
               return true;
#endif
            }
            void await_suspend(std::coroutine_handle<>) {
#ifdef INITIAL_AWAIT_SUSPEND_THROWS
                throw X{};
#endif
            }
            void await_resume() {
             PRINT("initial await_resume()");
#ifdef INITIAL_AWAIT_RESUME_THROWS
// This should be caught by unhandled_exception ()
                throw X{};
#endif
            }
        };

        awaiter initial_suspend() {
           PRINT("initial_suspend()");
#ifdef INITIAL_SUSPEND_THROWS
            throw X{};
#endif
            return {};
        }

        task get_return_object() {
            PRINT("get_return_object()");
#ifdef GET_RETURN_OBJECT_THROWS
            throw X{};
#endif
            return task{};
        }

        std::suspend_never final_suspend() noexcept { 
           PRINT("final_suspend()");
           return {};
        }
        void return_void() {
          PRINT("return_void()");
        }
        void unhandled_exception()  {
            PRINT("unhandled_exception()");
            unhandled_exception_called++;
#ifdef UNHANDLED_EXCEPTION_THROWS
	    Z z{std::current_exception(),coro_handle::from_promise(*this)};
	    throw z;
            //throw coro_handle::from_promise(*this);
#endif
        }
    };
   
    task() noexcept {
     PRINT("task()");
     task_live++;
    }
    task(task&& t) noexcept {
      PRINT("task(task&&)");
      task_live++;
    }
    ~task() noexcept {
      PRINT("~task()");
      task_live--;
    }

};

task f(Y Val __attribute__((__unused__))) {
  PRINT("f()");
#ifdef BODY_THROWS
  throw X{};
#endif
  co_return;
}

int main() {
  bool failed = false;
  Y Val;
  try {
    PRINT("try-block-s");
    task tsk = f(Val);
    PRINT("try-block-e");
  } catch (X) {
    PRINT("caught X");
  } catch (task::Z& z) {
    PRINTF("caught Z : %d\n",(bool)z.eptr);
    if (z.h.done())
      {
        PRINT("done, cleaning up");
        z.h.destroy ();
      }
    else
      PRINT("not done?");
  } catch (std::coroutine_handle<task::promise_type>& h) {
    PRINT("caught handle");
    if (h.done())
      {
        PRINT("done, cleaning up");
        h.destroy ();
      }
    else
      PRINT("not done?");
  }

  if (task_live)
    failed = true, __builtin_printf("task still live\n") ;
  if (promise_live)
    failed = true, __builtin_printf("promise still live\n");
  if (frame_live)
    failed = true, __builtin_printf("frame still live : %d\n", frame_live);
  if (Y_live != 1)
    failed = true, __builtin_printf("Y live count : %d\n", Y_live);
#ifdef SHOULD_CALL_UNHANDLED_EXCEPTION
  if (unhandled_exception_called != 1)
    failed = true, __builtin_printf("unhandled exception : %d\n", unhandled_exception_called);
#else
  if (unhandled_exception_called != 0)
    failed = true, __builtin_printf("unhandled exception : %d\n", unhandled_exception_called);
#endif

  __builtin_printf("checking result : %s \n",(failed?"bad":"good"));

  fflush(stdout);

  if (failed)
   __builtin_abort();
}
