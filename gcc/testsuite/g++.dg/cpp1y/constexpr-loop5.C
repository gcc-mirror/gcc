// PR c++/68530
// { dg-do compile { target c++14 } }

struct thing {
    void foo() {}
};

template<typename>
constexpr int count()
{
    auto item = thing {};
    for(; (item.foo(), false);); // { dg-error "foo" }
    return 0;
}

int main()
{
    static_assert( count<int>() == 0, "" ); // { dg-error "" }
}
