// PR c++/14083

struct A { 
  A() throw() { } 
  A(const A&) throw() { } 
}; 
 
struct X { 
  A a; 
  X(); 
  X& operator=(const X& __str); 
}; 
 
bool operator==(const X& __lhs, const char* __rhs); 
        
int main() { 
  X x; 
  x=="" ? x : throw 1; 
}
