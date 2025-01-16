// PR rtl-optimization/113994
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

void
foo (const std::string &x, size_t &y, std::string &z)
{
  size_t w = x.find (']');
  if (w >= x.size ())
    return;
  else if (w == 1)
    y = std::string::npos;
  while (++w < x.size () && x[w] == u'.')
    ;
  z = x.substr (w);
}

__attribute__((noipa)) void
bar ()
{
}

int
main ()
{
  size_t y = 0;
  std::string z;
  foo ("[0]", y, z);
  bar ();
}
