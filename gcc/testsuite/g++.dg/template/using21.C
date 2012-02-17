// PR c++/52126
// { dg-do compile }

template<typename T>
struct A
{
    int foo;

    struct B : A<T>
    {
        using A::foo;
    };

    struct C : A
    {
        using A::foo;
    };

    struct D : A<T>
    {
	using A<T>::foo;
    };

    struct E : A
    {
	using A<T>::foo;
    };
};
