// PR c++/95497
// { dg-do compile { target c++20 } }

template <typename T>
struct A{};

template <typename T>
concept c =
    requires(T t, A<int> b) // note that A<int> is independent of T
    {
        { t += b };
    };
