// { dg-do compile }

// Origin: Richard Guenther <rguenth@tat.physik.uni-tuebingen.de>

// PR c++/12924

template<typename> struct A {};

template<> struct A<void>
{
    template<typename T> void foo()
    {
        A<T> a;
        a.template foo<int>();	// { dg-error "no member" }
    }
};

void bar()
{
    A<void> a;
    a.foo<int>();		// { dg-error "instantiated" }
}
