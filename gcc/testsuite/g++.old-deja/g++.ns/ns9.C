// { dg-do assemble  }
namespace bb
{
  int f(int);

  namespace k
  {
    void foo(int bar)
    {
      return bb:f(bar); //{ dg-error "" } syntax error
    }
  }
}
