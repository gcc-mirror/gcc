// PR c++/15629
// { dg-do link }

template<int a, int b> class T; 
 
template<int a, int b> void func(T<a, b> * t);
template<int a>        void func(T<a, 3> * t) {}
template void func<2>(T<2, 3>*);
 
template<int a, int b> struct T { 
  friend void func<a, b>(T<a, b> * t); 
  friend void func<a>   (T<a, 3> * t); 
   
  void foo(); 
}; 
 
template<int a, int b> void T<a, b>::foo() { 
  func((T<2,3>*)0); 
} 
 
int main() { 
  T<2,3>().foo(); 
} 
