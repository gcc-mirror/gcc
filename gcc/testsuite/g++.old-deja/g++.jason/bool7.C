// Build don't link:

struct A
{
  operator bool () const;
  operator const void * () const;
};

struct B
{
  A a;
  int foo1 ();
  int foo2 ();
};

int
B::foo1 ()
{
  return a ? 0 : 1;  // ambiguous default type conversion for `operator !='
}

int
B::foo2 ()
{
  if (a)
    return 0;
  else
    return 1;
}
