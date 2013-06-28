/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining" } */

class H
{
public:
  virtual unsigned bar() const { return 16; }
};

class A : public H
{
  unsigned foo(unsigned (H::*func)(void) const) const;
public:
  H *h;
  virtual unsigned bar() const;
};

unsigned A::foo(unsigned (H::*func)(void) const) const
{
  return  (h->*func)();
}

unsigned A::bar() const
{
  return foo(&H::bar);
}

int main (int argc, char **argv)
{
  H h;
  A a;
  a.h = &h;

  if (a.bar() != 16)
    __builtin_abort ();
  return 0;
}
