// Testcase for instantiation with cv-qualified type
// Build don't link:

template<class T>
struct A
{
    void foo();
};

template<class T> void A<T>::foo() { }

template class A<const int>;
