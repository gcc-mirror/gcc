// PR c++/88757
// { dg-do compile { target c++11 } }

template <class T> struct C {
    static int x;
};
template <class U> struct S {
    static const int size = 1;
};
template <class T> int C<T>::x(S<T>::size);
