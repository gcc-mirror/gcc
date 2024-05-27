// { dg-do compile }
// { dg-require-effective-target c++20 }

using size_t = decltype(sizeof(0));
template <class U>
static constexpr bool cst = true;
template<class T>
struct Outer
{
    Outer();
    template <class U> void method() requires cst<U>
    {
        struct Inner
        {
            static void* operator new(size_t){return new char;}
            static void operator delete(void*){}
            Outer<void> t;
        };
        new Inner;
    }
};
void f()
{
    Outer<void>{}.method<void>();
}
