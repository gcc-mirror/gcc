// { dg-do compile }

// Origin: heinlein@informatik.uni-ulm.de

// PR c++/14429: Matching of template template parameter containing
// non-type parameter with type that depends on earlier parameter.

template <template <typename U, U* p> class T>
struct X {};

template <template <typename U, U* p> class T>
struct Y {
    X<T> x;
};
