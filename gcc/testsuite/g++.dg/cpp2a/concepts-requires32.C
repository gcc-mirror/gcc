// PR c++/101733
// { dg-do compile { target c++20 } }

template<class T>
requires requires {
    typename T::type;
    (typename T::type()); // (1)
    T::type();            // (2)
    typename T::type();   // (3)
}
void f(T) { }
