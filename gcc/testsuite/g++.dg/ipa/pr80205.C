// PR ipa/80205
// { dg-options "-fnon-call-exceptions --param early-inlining-insns=100 -O2" }

class a
{
public:
  virtual ~a ();
};
class b
{
public:
  template <typename c> b (c);
  ~b () { delete d; }
  void
  operator= (b e)
  {
    b (e).f (*this);
  }
  void
  f (b &e)
  {
    a g;
    d = e.d;
    e.d = &g;
  }
  a *d;
};
void
h ()
{
  b i = int();
  void j ();
  i = j;
}
