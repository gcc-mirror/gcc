// Bug: g++ fails to grok functional casts in all situations.
// Build don't link:

class A  {
public:
  typedef int B;
  static B foo() { return B(1); } // gets bogus error - 
};
