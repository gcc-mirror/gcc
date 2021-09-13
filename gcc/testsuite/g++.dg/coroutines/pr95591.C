#if __has_include (<coroutine>)
#include <coroutine>
using namespace std;
#elif defined (__clang__) && __has_include (<experimental/coroutine>)
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif

struct generator {
    struct promise_type {
        generator get_return_object();
        void return_void();
        void unhandled_exception();
        suspend_always initial_suspend();
        suspend_always final_suspend() noexcept;

        template<typename Arg>
        suspend_always yield_value(Arg&&) {
            return {};
        }
    };
};

generator x() {
    co_yield "foo";
}
