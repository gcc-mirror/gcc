// { dg-lto-do run }
// { dg-lto-options { { -O2 -flto } } }

#include "pr126194.h"

extern "C" void* malloc(Size size) noexcept;
extern "C" void free(void* p) noexcept;

namespace {
class M final : public R
{
public:
    void* allocate(Size size, const void* /*caller*/) override
    {
        return malloc(size);
    }

    void deallocate(void* p, const void* /*caller*/) noexcept override
    {
        free(p);
    }
};

M g_r;
}

R& get_r() noexcept
{
    return g_r;
}

void touch(const void* p) noexcept
{
    static const volatile void* sink = nullptr;
    sink = p;
}

void release_q(void* p)
{
    free(p);
}

int g_should_throw = 0;

void* A::create(Size size, const void* caller, R& r) const
{
    reset();
    if (size < a_) size = a_;
    void* p = r.allocate(size, caller);
    if (!p) throw 2;
    *p_ = p;
    return p;
}

__attribute__((noinline))
void A::destroy(void* p, const void* caller, R& r) const noexcept
{
    *p_ = nullptr;
    r.deallocate(p, caller);
}

void* operator new(Size size, A const& a, R& r) noexcept(false)
{
    return a.create(size, __builtin_return_address(0), r);
}

__attribute__((noinline))
void operator delete(void* p, A const& a, R& r) noexcept
{
    a.destroy(p, __builtin_return_address(0), r);
}
