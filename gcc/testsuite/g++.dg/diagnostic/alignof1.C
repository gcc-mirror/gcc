struct A
{
  int foo() { return __alignof(bar); } // { dg-error "32:ISO C\\+\\+ forbids applying .__alignof." }
  int bar();
};
