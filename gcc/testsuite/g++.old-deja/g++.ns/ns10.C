// { dg-do assemble  }
namespace bb
{
  int f(int);

  namespace k
  {
    void foo(int bar)
    {
      int i=bb:f(bar); // { dg-error "" } namespace
    }
  }
}
