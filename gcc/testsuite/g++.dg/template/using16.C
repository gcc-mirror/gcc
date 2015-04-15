// { dg-do compile }

template <class T>
struct A
{
    typedef T type;
};

template <class T>
struct B
{
    class type
    {
	type(); // { dg-message "private" }
    };
};

template <class T>
struct C : A<T>, B<T>
{
    using typename B<T>::type;

    void f()
    {
	type j; // { dg-error "context" }
    }
};

template class C<int>; // { dg-message "required" }

template <class T>
struct D
{
    typedef T type;
};

template <class T>
class E : D<T>
{
    using typename D<T>::type; // { dg-message "previous" }
    using typename D<T>::type; // { dg-error "redeclaration" }
};
