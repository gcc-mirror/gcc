// Origin PR c++/47172
// { dg-do compile { target c++11 } }

struct A
{
    int f() const;
};

template <class T>
struct B : A { };

template <class T>
struct C : B<T>
{
    void g();
};

template <class T>
void C<T>::g()
{
    A::f();
}
