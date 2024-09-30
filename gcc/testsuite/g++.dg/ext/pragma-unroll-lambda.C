// { dg-do compile { target c++11 } }

template<typename Iter, typename Pred>
inline Iter
my_find(Iter first, Iter last, Pred pred)
{
#pragma GCC unroll 4
    while (first != last && !pred(*first))
        ++first;
    return first;
}

short *use_find(short *p)
{
    auto pred = [](short x) { return x == 42; };
    return my_find(p, p + 1024, pred);
}
