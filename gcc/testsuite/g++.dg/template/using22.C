// PR c++/52126
// { dg-do compile }

template <class T> struct Z {};

template<typename T>
struct A
{
    struct B : A<T>
    {
        using A::nonexist; // { dg-error "no members matching" }
    };

    struct C : A
    {
        using A::nonexist; // { dg-error "no members matching" }
    };

    struct D : A<T>
    {
    	using A<T>::nonexist; // { dg-error "no members matching" }
    };

    struct E : A
    {
    	using A<T>::nonexist; // { dg-error "no members matching" }
    };

    struct F : Z<T>
    {
	using Z<T>::nonexist;
    };
};
