//  { dg-additional-options  "-w" }

#include "coro.h"

using namespace std;

struct task {
    struct promise_type {
        promise_type() {}
        task get_return_object() { return {}; }
        suspend_never initial_suspend() { return {}; }
        suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
    };
};

task foo(int) {
    co_return;
}
