// PR c++/15299

template <class T>  void fun_ptr(T (*)()); 
template <class T>  T    bar(); 
 
template <class> void foo () { 
  fun_ptr(bar<int>); 
} 
