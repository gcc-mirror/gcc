// { dg-options "-Wnon-virtual-dtor" }

extern "Java"
{
  class Foo
  {
  public:
    virtual void bar( void);
  };
}
