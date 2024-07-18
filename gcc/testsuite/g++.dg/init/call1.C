// Bug c++/16115
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort(); 
 
int count = 0; 
 
struct T { 
    T() { count++; } 
    T(const T&) { count++; } 
    ~T() { if (count==0) abort(); --count; } 
}; 
 
struct auto_ptr { 
    T* p; 
 
    auto_ptr(T* __p) : p(__p) { } 
    ~auto_ptr() { delete p; } 
 
    T* release() { 
      T* t = p; 
      p = 0; 
      return t; 
    } 
}; 
 
void destroy (auto_ptr a) { 
  delete a.release(); 
} 
 
 
int main () 
{ 
  destroy (new T); 
}
