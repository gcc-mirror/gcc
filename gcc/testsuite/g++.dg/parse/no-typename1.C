// PR c++/4903
// Origin: Dan Marinescu <theverylittleone@hotmail.com>
// { dg-do compile }

template <typename T> struct A
{
    template <typename U> struct B
    {
        A<T>::template B<U> foo();
    };
};
