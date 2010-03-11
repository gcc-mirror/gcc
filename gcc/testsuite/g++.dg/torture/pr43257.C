/* { dg-do assemble } */

class A {};
class B {};

static void *func (int n)
{
  void *p;
  if (p == 0) throw ::A ();
}

static void *func (int n, B const &)
{
  try {
      return func (n);
  }
  catch (::A const &) {
  }
  return func (n);
}

void *f1 (int n)
{
  return func (n, B());
}

void *f2 (int n)
{
  return func (n, B());
}
