// PR c++/105321
// { dg-do compile { target c++11 } }

bool handle_error();

constexpr int echo(int value, bool yes = true) noexcept
{
    return (yes || handle_error()), value;
}

static_assert(echo(10) == 10, "");

constexpr int echo2(int value, bool no = false) noexcept
{
    return (!no || handle_error()), value;
}

static_assert(echo2(10) == 10, "");
