// { dg-do assemble  }
namespace foo
{
  class c
  {
  };
}

int
foo::c:fn ()  // { dg-error "" } syntax error
{
}

