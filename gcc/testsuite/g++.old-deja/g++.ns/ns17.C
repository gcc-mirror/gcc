//Build don't link:
namespace foo
{
  class c
  {
  };
};

int
foo::c:fn ()  // ERROR - syntax error
{
}

