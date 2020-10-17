#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif

struct dummy_coroutine {};

namespace std {

template<>
class coroutine_traits<::dummy_coroutine> {
public:
    struct promise_type {
        void return_value(int x) {  }
        void return_void() {}
        std::suspend_never initial_suspend() noexcept { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        dummy_coroutine get_return_object() { return {}; }
        void unhandled_exception() {}
    };
};

}

dummy_coroutine
foo() { // { dg-error {the coroutine promise type 'std::__n4861::coroutine_traits<dummy_coroutine>::promise_type' declares both 'return_value' and 'return_void'} }
    co_return 17;
}
