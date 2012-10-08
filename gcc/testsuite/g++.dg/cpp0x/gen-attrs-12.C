// { dg-do compile { target c++11 } }
// PR c++/13791

template <typename T> struct O {
  struct [[gnu::packed]] I {
    int i;
    char c;
  };

  I* foo();
};

template <typename T>
typename O<T>::I*
O<T>::foo() { return 0; }

template class O<int>;
