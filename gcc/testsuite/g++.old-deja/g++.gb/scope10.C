// Build don't link: 
// GROUPS passed gb scope
void Foo (void)
{
  class C {
  public:
    virtual int foo (void) { return 0; };
  };
}
