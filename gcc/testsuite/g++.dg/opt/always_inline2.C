// PR c++/120395
// { dg-additional-options "-fdump-tree-optimized" }
// { dg-final { scan-tree-dump-not "goto" "optimized" } }
// { dg-do compile { target c++11 } }

void x(int);

[[gnu::always_inline]] constexpr bool
is_constant_evaluated()
{ return __builtin_is_constant_evaluated(); }

struct Iter
{
    typedef int value_type;

    int& operator*() const;
    Iter& operator++();
    bool operator!=(const Iter&) const;
};

void f(Iter first, Iter last)
{
    if (__is_trivial(Iter::value_type))
        if (!is_constant_evaluated())
            return;
    for (; first != last; ++first)
        x(*first);
}
