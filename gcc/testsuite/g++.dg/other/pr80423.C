// PR middle-end/80423
// { dg-do compile { target c++11 } }

typedef unsigned char uint8_t;                                                                                                                     
struct A {                                                                                                                                         
  template <int N> A(unsigned char (&)[N]);                                                                                                        
};                                                                                                                                                 
void fn1(A) {                                                                                                                                      
  uint8_t a[]{0};                                                                                                                                  
  fn1(a);                                                                                                                                          
}                                                                                                                                                  
