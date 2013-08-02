// PR c++/58022

template <class T> struct A { };
template <class T> A<T> & operator<< (A<T>&, T);
template <class T> class foo;
template <class T> A<char> & operator<<(A<char>& o, const foo<T>& l);
template <class T> class foo  {
    friend A<char>& operator<< <T> (A<char>& o, const foo<T>& l);
};
class bar;
foo<bar> fb;
class bar { virtual void baz()=0; };
