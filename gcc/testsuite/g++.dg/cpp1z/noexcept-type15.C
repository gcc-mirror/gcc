// PR c++/80384
// { dg-do compile { target c++17 } }

template<class> struct foo;
template<bool B>
struct foo<int() noexcept(B)> {
    static const bool value = B; 
};

static_assert(!foo<int()>::value);
static_assert(foo<int() noexcept>::value);
