// PR c++/115657
// { dg-do compile { target c++11 } }

struct NonIntegral
{
    constexpr operator int() { return 0; }
};

template<typename T> struct TemplatedStructural
{
    enum { e = NonIntegral{} };
};

template struct TemplatedStructural<void>;
