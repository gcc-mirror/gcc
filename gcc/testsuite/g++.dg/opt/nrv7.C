// PR c++/15461

struct A {  
    int i;  
};  
  
inline A foo () {  
    int j = 1; 
    A a = { j }; 
    return a;  
} 
 
A tv = foo();
