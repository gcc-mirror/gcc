// Make sure we can overload on ObjC classes
// Radar 3960754

// { dg-do compile }

@class A, B; 

struct X {
  void call(A*);
  void call(B*);
};
