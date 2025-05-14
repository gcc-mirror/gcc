// PR c++/109431
// { dg-do run { target c++11 } }

struct RangeLimits
{
    int low = 0;
    int high = 1;
    constexpr RangeLimits() { }
};

template <int>
int parameterLimits(void)
{
    static RangeLimits constexpr param_limits[2] = {};
    if (param_limits[0].low != 0
	|| param_limits[0].high != 1
	|| param_limits[1].low != 0
	|| param_limits[1].high != 1)
      __builtin_abort ();
    auto const &limits = param_limits[1];
    return 0;
}

auto s = parameterLimits<1>();

int
main ()
{
}
