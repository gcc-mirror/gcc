// PR c++/87750
// { dg-do compile { target c++11 } }

template <typename T>
class Bar
{
protected:
    template <bool B>
    int process(int) { return 0; }
};

template<typename T>
class Derived : Bar<T>
{
    using Base = Bar<T>;
    // Note applying Base::template workaround in (2) and commenting
    // this out then compiles.
    using Base::process;
public:
    void foo()
    {
        // (1) workaround: this->template
        // This line only fails on gcc 8.x, works in clang/icc/msvc.
        process<false>();
    }

    template <bool B>
    int process()
    {
        // (2) workaround: this->template or Base::template
        // Note clang 5 & 6 don't accept this line either, but clang 7 does.
        return process<B>(1);
    }
};

int main()
{
    Derived<int> x;
    return x.process<false>();
}
