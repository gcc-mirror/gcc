// { dg-do assemble  }
// GROUPS passed gb scope
void Foo (void)
{
  class C {
  public:
    virtual int foo (void) { return 0; };
  };
}
