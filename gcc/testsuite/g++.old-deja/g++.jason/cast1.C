// { dg-do assemble  }
// Bug: g++ fails to grok functional casts in all situations.

class A  {
public:
  typedef int B;
  static B foo() { return B(1); } // { dg-bogus "" } 
};
