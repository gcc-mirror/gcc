// PR c++/65876
// { dg-do compile { target c++11 } }

template<int>
struct duration
{
    constexpr duration() : r(0) {}

    template<int TPeriod>
    constexpr duration(duration<TPeriod> x) : r(x.count()) {}

    constexpr int count() { return 0; }

    int r;
};

struct Config {
    duration<1> timeout { duration<2>() };
};

Config make_config()
{
    return {};
}

struct ConfigArray {
    ConfigArray();
    Config all_configs[1];
};

ConfigArray::ConfigArray()
{
}
