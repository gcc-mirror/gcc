// { dg-do compile }

// Origin: lorgon1@yahoo.com

// PR c++/11154: Multi-level template argument in partial ordering of
// class template

template <class A> struct Outer {
   template <class T, class U = void, class V = void> struct Foo {};
   template <class T, class U> struct Foo<T,U,void> {};
   template <class T> struct Foo<T,void,void> {};
};

Outer<int>::Foo<int,void,void> f;
