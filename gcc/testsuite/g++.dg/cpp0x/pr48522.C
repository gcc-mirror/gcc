// { dg-options "-std=c++0x" }

template <typename T>
struct Handle
{
    Handle(T& t);
};

template<class T>
struct Class {
    struct Struct {} data;
    void f();
    void g();
};

template<class T>
void Class<T>::f() {
    Handle< decltype((data)) > handle(data);
}

template<class T>
void Class<T>::g() {
    Handle< decltype((data)) > handle(data);
}
