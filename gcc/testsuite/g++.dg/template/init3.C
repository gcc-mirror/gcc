// PR c++/11027

template <class T>
struct X {
   typedef void (X::*pfun)();
   static pfun p[];
};

template <class T>
typename X<T>::pfun X<T>::p[] = {};

