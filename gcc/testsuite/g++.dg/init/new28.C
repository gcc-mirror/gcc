// PR c++/43951
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

class Foo 
{
public:
  Foo () : xxx (1) {};
  const int xxx;
};
Foo *
bar ()
{
  return new Foo;
}

int
main ()
{
  Foo *p = bar ();

  if (p->xxx != 1)
    abort ();
  return 0;
}
