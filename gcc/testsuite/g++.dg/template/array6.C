// PR c++/15287

struct S {}; 
 
struct Array { 
  S operator[](int); 
} array; 
 
void (S::*mem_fun_ptr)(); 
 
template <int> void foo() { 
  (array[0].*mem_fun_ptr)(); 
} 
