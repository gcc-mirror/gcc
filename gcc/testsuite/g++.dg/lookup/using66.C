// PR c++/92918
// { dg-do compile { target c++11 } }

struct Base03
{
    static void impl();
};

struct Problem : Base03
{
    using Base03::impl;
    static int impl(char const *);

    template <typename T>
    auto f(const T &t) const
    -> decltype(impl(t))
    {
        return impl(t);
    }
};

Problem t;
int i = t.f("42");
