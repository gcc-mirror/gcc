// PR c++/67838
// { dg-do compile { target c++14 } }

template<bool LMode>
static auto TestFunc = [](int param1)
{
    return param1;
};

template<typename Func>
static void test(Func func)
{
    func(12345);
}

int main()
{
    test(TestFunc<false>);
    test(TestFunc<true>);
}
