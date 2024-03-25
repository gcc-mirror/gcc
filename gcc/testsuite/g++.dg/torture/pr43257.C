/* { dg-do assemble } */
/* Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.  */

class A {};
class B {};

static void *func (int n)
{
  void *p;
  if (p == 0) throw ::A ();
}	// { dg-warning "control reaches end of non-void function" }

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
