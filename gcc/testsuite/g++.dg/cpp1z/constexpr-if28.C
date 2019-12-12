// PR c++/89576
// { dg-do compile { target c++17 } }

template <class T>
void foo()
{
    constexpr int x = 0;
    [&] {
        if constexpr (x) {}
    };
}
