// PR c++/90754 ICE in type lookup.

class A {
  struct COMTypeInfo;
};
class B {
  struct COMTypeInfo;
};
class C : A, B {
  struct COMTypeInfo;
};
