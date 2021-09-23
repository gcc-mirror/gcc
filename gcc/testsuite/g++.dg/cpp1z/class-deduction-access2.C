// { dg-do compile { target c++17 } }

struct B {
protected:
    struct type {};
};
template<typename T> struct D : B {
    D(T, typename T::type);
};
D c = {B(), {}};
