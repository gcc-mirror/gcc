class S  
{ 
public: 
  S(){} 
};  
  
int foo(char* m1) {  
  throw (m1 ? S() : S()); 
} 
