// PR c++/12397

struct foo { }; 
 
template <typename T> struct bar 
{ 
  bar(){} 
  int i; 
  bar (const bar<T>& foo) : i (foo.i) {} 
}; 
 
int main() 
{ 
  bar<int> b1; 
  bar<int> b2(b1); 
} 
