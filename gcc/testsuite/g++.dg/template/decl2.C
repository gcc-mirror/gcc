// PR c++/16162

template <int N> struct O { 
  struct I { 
    template <typename T> struct II { 
      void f(); 
    }; 
  }; 
}; 
 
template <int N> 
template <typename T> 
void O<N>::I::II<T>::f () {} 
