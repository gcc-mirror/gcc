// { dg-do compile }

// Origin: <schmid@snake.iap.physik.tu-darmstadt.de>

// Bug: Overloading of ordinary and template member function
// which enclosing class is specialized is not handled correctly.

template <class T>
struct A
{
    void f(T) {}
};

template<>
struct A<int>
{
    void f(int) {}
    template <class T> void f(T) {}
};

template
void A<int>::f(int);
