// { dg-do compile }

// Origin: <schmid@snake.iap.physik.tu-darmstadt.de>

// Bug: ICE during invalid instantiation of member function
// which enclosing class is specialized.

template <class T>
struct A
{
    void f(T) {}
};

template<>
struct A<int>
{
    void f(int) {}
};

template
void A<int>::f(int);		// { dg-error "not match" }
