//Build don't link:
namespace bb
{
  int f(int);

  namespace k
  {
    void foo(int bar)
    {
      int i=bb:f(bar); // ERROR - namespace
    }
  }
}
