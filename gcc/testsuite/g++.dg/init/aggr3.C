// PR c++/18793

struct S { 
  S(); 
  S(const S&); 
  void operator=(const S&); 
}; 
 
struct X { 
  int a, b, c, d, e; 
  S s; 
}; 
 
void foobar () { 
  X x = {0}; 
} 
