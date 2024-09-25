//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

struct pt_b
{
    std::suspend_never initial_suspend() const noexcept { return {}; }
    std::suspend_never final_suspend() const noexcept { return {}; }
    void unhandled_exception() const noexcept {}
};

int called_rv_op = 0;
struct rv
{
    void operator ()(){
        PRINT("call to operator ");
        called_rv_op++;
    }
};

struct pt_c : pt_b
{
    using handle_t = std::coroutine_handle<pt_c>;
    auto get_return_object() noexcept { return handle_t::from_promise(*this); }
    rv return_void;
};

int called_lambda = 0;

struct pt_d : pt_b
{
    using handle_t = std::coroutine_handle<pt_d>;
    auto get_return_object() noexcept { return handle_t::from_promise(*this); }
    static constexpr auto return_void
      = []{ PRINT("call to lambda "); called_lambda++; };
};

template <> struct std::coroutine_traits<pt_c::handle_t>
    { using promise_type = pt_c; };

static pt_c::handle_t foo ()
{
    co_return;
}

template <> struct std::coroutine_traits<pt_d::handle_t>
    { using promise_type = pt_d; };

static pt_d::handle_t bar ()
{
    co_return;
}

int main ()
{
    foo ();
    bar ();
    if (called_rv_op != 1 || called_lambda != 1)
      {
        PRINT ("Failed to call one of the return_void cases");
        abort ();
      }
}
