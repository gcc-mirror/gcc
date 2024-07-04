// { dg-do assemble  }
// PRMS Id: 6825

class aClass 
{ 
  ; // { dg-error "" "" { target c++98_only } } missing declaration
private: 
  ; // { dg-error "" "" { target c++98_only } } missing declaration
}; 
