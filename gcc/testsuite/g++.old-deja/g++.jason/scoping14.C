// { dg-do assemble  }
// [class.scope0]: The scope of a name declared in a class consists
// ... also of all ... default arguments ... in that class ....

struct A {
  void f (int A::* = &A::i);	
  int i;
};
