// { dg-do assemble  }
// Bug: g++ can't deal with function-local classes that talk about themselves.

void foo() {
  class Wrapper {
  public:
    void F (void * Wrapperptr)
    {
      Wrapper * wrapptr = (  Wrapper  *) Wrapperptr; // { dg-bogus "" }  
    }
  };
}
