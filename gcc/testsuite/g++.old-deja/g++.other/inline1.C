// { dg-do assemble  }
// { dg-options "-O2 -Winline" }
// Origin: Martin Reinecke <martin@MPA-Garching.MPG.DE>

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
