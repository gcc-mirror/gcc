// PR c++/16518

struct foo { 
  void bar() const { m1=1; m2=1;} 
  mutable int m1,m2; 
}; 
 
