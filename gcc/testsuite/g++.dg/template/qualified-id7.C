// PR c++/85731
// { dg-do compile { target c++11 } }

    template <typename T>
    struct Outer {
        struct Inner;
        template <int I> static void f();
    };

    template <typename T>
    struct Outer<T>::Inner {
        decltype(Outer<T>::f<42>()) f();
    };

    int main() { Outer<int>::Inner().f(); }
