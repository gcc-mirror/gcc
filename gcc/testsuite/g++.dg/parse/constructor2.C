// PR c++/14260

template <class TClass> 
class T 
{ 
public: 
  T(short,short f=0) {} 
  T<TClass>(int f) {} // { dg-error "expected" "" { target c++20 } }
  T<TClass>(int f=0,const char* b=0) {} // { dg-error "expected" "" { target c++20 } }
}; 

