// Build don't link:
// Origin: Martin Reinecke <martin@MPA-Garching.MPG.DE>
// Special g++ Options: -O2 -Winline

class foo
{
  public:
    float x;

    foo (float xval)
      : x (xval) {}

    foo operator+ (const foo &foo2) const
      { return foo (x+foo2.x); }
};

int main()
{
  foo f=foo(1)+foo(2);
}
