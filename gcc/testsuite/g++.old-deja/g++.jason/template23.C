// { dg-do assemble  }
// Testcase for instantiation with cv-qualified type

template<class T>
struct A
{
    void foo();
};

template<class T> void A<T>::foo() { }

template class A<const int>;
