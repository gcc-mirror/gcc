// PR c++/97297
// { dg-do compile { target c++20 } }

template <typename T>
struct S {
    int simple(T::type);

    template <typename U>
    int member(U::type);
};

template <typename T>
int S<T>::simple(T::type) {
    return 1;
}

template <typename T>
template <typename U>
int S<T>::member(U::type) {
    return 2;
}
