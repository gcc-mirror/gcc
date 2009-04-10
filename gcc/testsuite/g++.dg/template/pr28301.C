// PR c++/28301
// { dg-do compile }

template<typename> struct A
{
    template<int> void foo()
};	// { dg-error "initializer" }

template<> struct A<void>
{
    template<int> void foo();
};

void bar()
{
    A<void> a;
    a.foo<0>();
}
