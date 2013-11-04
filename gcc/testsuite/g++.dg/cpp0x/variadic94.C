// PR c++/40595
// { dg-options "-std=c++11" }

template<int N>
struct S
{
    typedef int type;
};

template<typename T>
struct Get
{
    static T get();
};

template<typename F>
struct B
{
    template<typename ... Args>
        typename S<sizeof( Get<F>::get() (Get<Args>::get() ...) )>::type
        f(Args&& ... a);
};

struct X
{
    bool operator()(int) const;
};

int main()
{
    B<X> b;
    b.f(1);
}
