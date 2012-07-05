// PR c++/53524

template <class T> struct A { enum EA { ea }; };
template <class T, class U> struct B {
  enum EB { eb1 = A<T>::ea, eb2 = A<U>::ea, eb3 = 0 ? eb1 : eb2  };
};

B<int,char> b;
