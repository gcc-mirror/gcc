// { dg-options "-fabi-version=0" }

template <typename T> struct S {
    struct I{};
    operator I* ();
};

template <typename T> struct S2 : S<T> {
    operator typename S<T>::I* ();
};

template struct S2<int>;
