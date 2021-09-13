// PR c++/99318
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
    [[deprecated("foo")]] unsigned m_fn (char const chr)
    {
        using index_t = unsigned;
        return T::arr[static_cast<index_t>(chr)]; // { dg-bogus "deprecated" }
    }
};

extern unsigned int arr[];

struct R {
    [[deprecated("foo")]] unsigned m_fn (char const chr)
    {
        using index_t = unsigned;
        return arr[static_cast<index_t>(chr)]; // { dg-bogus "deprecated" }
    }
};
