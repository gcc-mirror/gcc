// Origin PR c++/47172
// { dg-options "-std=c++0x" }
// { dg-do compile }

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
