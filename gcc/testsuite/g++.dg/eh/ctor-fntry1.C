// PR c++/61611
// { dg-do run }

struct A { };
struct B { };

struct Test
{
  Test()
  try { throw A(); }
  catch(const A&)
    {
      try { throw B(); }
      catch(const B&) { }
    }
};

int
main()
{
  try { Test x; }
  catch(const A&) { }
}
