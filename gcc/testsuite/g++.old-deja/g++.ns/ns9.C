// Build don't link:
namespace bb
{
  int f(int);

  namespace k
  {
    void foo(int bar)
    {
      return bb:f(bar); //ERROR - syntax error
    }
  }
}
