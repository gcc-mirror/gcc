// PR c++/92918
// { dg-do compile { target c++11 } }

struct Base03
{
    static void impl();
};

struct ThisDoesNotCompileOnGCC : Base03
{
    using Base03::impl;
    static int impl(char const *);

    auto f(const char *t) const
    -> decltype(impl(t))
    {
        return impl(t);
    }
};

ThisDoesNotCompileOnGCC t;
int i = t.f("42");
