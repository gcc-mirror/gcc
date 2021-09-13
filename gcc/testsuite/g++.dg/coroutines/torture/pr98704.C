//  { dg-do run }
#include "../coro.h"

#include <stdexcept>

int frame_live = 0;
int promise_live = 0;
int task_live = 0;

struct Task
{
    struct promise_type;
    using handle = std::coroutine_handle<promise_type>;

    struct promise_type
    {
        promise_type () { promise_live++; PRINT ("promise_type ()"); }
        ~promise_type () { promise_live--; PRINT ("~promise_type ()"); }
        void* operator new(size_t sz) {
            PRINT("operator new()");
            frame_live++;
            return ::operator new(sz);
        }
        void operator delete(void* p, size_t sz) {
            PRINT("operator delete");
            frame_live--;
            return ::operator delete(p, sz);
        }

        Task get_return_object() { return handle::from_promise(*this); }
        auto initial_suspend() noexcept { return std::suspend_always{}; }
        auto final_suspend() noexcept { return std::suspend_always{}; }
        void return_void() noexcept {}

        auto yield_value(int x) noexcept
        {
            PRINTF ("yield_value(%d)\n", x);
            return std::suspend_always{};
        }

        void unhandled_exception()
        {
            PRINT ("unhandled_exception()");
            throw;
        }
    };

    Task(handle h) : coro(h) { task_live++; PRINT ("Task(handle h)"); }
    ~Task() { task_live--; PRINT ("~Task()"); if (coro) coro.destroy(); }

    handle coro;
};

Task myco()
{
    co_yield 42;
    throw std::out_of_range("TEST EXCEPTION");
}

int main()
{
  {
    Task task = myco();
    PRINT ("START");
    try {
        PRINTF ("done #0 = %d\n", task.coro.done());
        if (task.coro.done())
          abort();
        task.coro.resume(); // will yield 42
        PRINTF ("done #1 = %d\n", task.coro.done());
        if (task.coro.done())
          abort();        
        task.coro.resume(); // will throw exception
        PRINT ("should not be reached");
        abort ();
    }
    catch (const std::exception&) {
        PRINTF ("done exc = %d\n", task.coro.done());
        if (!task.coro.done())
          abort();        
    }
    if (!task.coro.done())
      abort();        
  } // should cause cause the destroy () to run.
  if (task_live || promise_live || frame_live)
    {
      PRINTF ("task_live = %d, promise_live = %d, frame_live = %d\n",
               task_live, promise_live, frame_live);
      abort ();
    }
}
