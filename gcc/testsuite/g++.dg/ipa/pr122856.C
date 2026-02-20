/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu++11" } */

template <typename T>
class Base {
public:
    virtual int get();
    virtual ~Base() = default;
};

template <typename T>
class Derived : public Base<T> {
public:
    int get() override { return Base<T>::get(); }
};

template <typename T>
int Base<T>::get() {

    return static_cast<Derived<int>*>(this)->get();
}

int main() {
    Derived<int> d;
    return d.get();
}
