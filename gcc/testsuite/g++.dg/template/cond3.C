// PR c++/13833

struct X { 
  template <typename T> 
  X & operator << (const T &t); 
  X & operator<< (int& (*p) (int&)); 
}; 
 
X x; 
 
template <int> void foo () { 
  x << (1 ? "ok" : "failed"); 
} 
 
template void foo<1>(); 
