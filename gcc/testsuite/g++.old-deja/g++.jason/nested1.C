// Bug: g++ can't deal with function-local classes that talk about themselves.
// Build don't link:

void foo() {
  class Wrapper {
  public:
    void F (void * Wrapperptr)
    {
      Wrapper * wrapptr = (  Wrapper  *) Wrapperptr; // gets bogus error 
    }
  };
}
