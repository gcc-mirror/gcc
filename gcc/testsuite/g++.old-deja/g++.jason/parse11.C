// { dg-do assemble  }
// PRMS Id: 6825

class aClass 
{ 
  ; // { dg-error "" } missing declaration
private: 
  ; // { dg-error "" } missing declaration
}; 
