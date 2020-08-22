//  { dg-do run }

#include "../coro.h"

struct pt_b
{
    std::suspend_never final_suspend() const noexcept { return {}; }
    constexpr void return_void () noexcept {};
    void unhandled_exception() const noexcept {}
};

int called_is_op = 0;
struct is
{
    std::suspend_never operator ()() noexcept {
        PRINT("call to operator IS");
        called_is_op++;
        return {};
    }
};

struct pt_c : pt_b
{
    using handle_t = std::coroutine_handle<pt_c>;
    auto get_return_object() noexcept { return handle_t::from_promise(*this); }
    is initial_suspend;
};

int called_lambda = 0;
struct pt_d : pt_b
{
    using handle_t = std::coroutine_handle<pt_d>;
    auto get_return_object() noexcept { return handle_t::from_promise(*this); }
    static constexpr auto initial_suspend
      = []() noexcept {
	   PRINT("call to lambda IS");
	   called_lambda++;
	   return std::suspend_never{};
	  };
};

template <>
struct std::coroutine_traits<pt_c::handle_t>
{ using promise_type = pt_c; };

static pt_c::handle_t foo ()
{
    co_return;
}

template <>
struct std::coroutine_traits<pt_d::handle_t>
{ using promise_type = pt_d; };

static pt_d::handle_t bar ()
{
    co_return;
}

int main ()
{
    foo ();
    bar ();
    if (called_is_op != 1 || called_lambda != 1)
      {
        PRINT ("Failed to call one of the initial_suspend cases");
        abort ();
      }
}
