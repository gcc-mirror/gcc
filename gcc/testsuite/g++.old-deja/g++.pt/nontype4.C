// Build don't link:

template <class R, void (R::* A) (void)>
class s
{
public:
  s (R &r) : _r (r) {}

  void e (void) { (_r.*A) (); }

private:
  R &_r;
};

class x
{
public:
  void test1 (void) { int j = 0; }
  void test2 (void) { int j = 1; }
};

int
main (void)
{
  x r;

  s<x, &x::test1> c4 (r);
  s<x, &x::test2> c5 (r);

  return 0;
}
