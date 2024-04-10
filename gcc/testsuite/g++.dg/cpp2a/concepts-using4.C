// PR c++/113498
// { dg-do compile { target c++20 } }

template<int d>
struct S_Base
{
    static constexpr int D = d;
};

template<int d>
struct S : public S_Base<d>
{
    using S_Base<d>::D;
    constexpr void f() const
        requires(D > 0) {}

};

int main(int, char**)
{
    S<1> s;
    s.f();
    return 0;
}
