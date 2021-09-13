/* PR c++/99251 - inconsistent -Wnonnull warning behaviour with dynamic_cast
   { dg-do compile }
   { dg-options "-Wall" } */

struct A
{
  virtual ~A ();
};

struct B: A
{
  int f (int);
};

int f1 (A *p)
{
  if (!p)
    return 0;

  return (dynamic_cast<B *>(p))->f (1);
}

int f2 (A *p)
{
  if (!p)
    return 0;

  return dynamic_cast<B *>(p)->f (2);   // { dg-bogus "\\\[-Wnonnull" }
}
