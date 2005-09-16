// PR c++/23896

template <int> struct X {}; 
 
template <typename T> struct length { 
  static const int value = 2; 
}; 
 
template <typename T> void foo () { 
  sizeof(X<length<T>::value>); 
} 
 
template void foo<int>(); 
