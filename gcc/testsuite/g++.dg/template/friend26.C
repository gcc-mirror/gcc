// PR c++/14359

template<typename> struct A {};

template<typename> struct B
{
    template<typename T> friend void foo(const A<T>& a, const B&) { a; }
};

void bar()
{
    A<void> a;
    B<void> b;
    foo(a,b);
}
