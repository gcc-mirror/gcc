#include "pr126194.h"

extern int g_should_throw;

struct T
{
    int a;
    __attribute__((noinline)) T() : a(0)
    {
        if (g_should_throw) {
            throw 1;
        }
    }
};

__attribute__((noinline))
int make_widget(bool go, void** out_ptr)
{
    B b;
    R& r = get_r();
    if (go) {
        T* t = new(b, r) T();
        (void)t;
    }
    *out_ptr = b.q;
    b.q = nullptr;
    return 42;
}

extern "C" __attribute__((visibility("default")))
int run_repro(int argc)
{
    g_should_throw = (argc > 0);
    void* p = nullptr;
    try {
        (void)make_widget(argc > 0, &p);
        return 1;
    } catch (...) {
        return 0;
    }
}
