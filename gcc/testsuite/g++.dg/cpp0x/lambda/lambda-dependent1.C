// PR c++/85815
// { dg-do compile { target c++11 } }

template<class T>
class A {
    static A* INSTANCE;
    void foobar();
    void moo() {}
};

template<class T>
A<T>* A<T>::INSTANCE = nullptr;

template<class T>
void A<T>::foobar() {
    auto x = []() {
        INSTANCE->moo();
    };
}
